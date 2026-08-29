#' Execution engine loop for all the fetch commands
#' @param self The R6 connection object.
#' @param msg_id A \code{numeric vector} containing one or more message ids.
#' @param fetch_request A string containing the fetch request to the server that
#'   will be added to the curl handle.
#' @param use_uid Default is \code{FALSE}. In this case, results will be
#'   presented as message's sequence numbers. A message sequence number is a
#'   message's relative position to the oldest message in the mailbox. It may
#'   change after deleting or moving messages. If a message is deleted,
#'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
#'   command will be performed using the \code{"UID"} or unique identifier,
#'   and results are presented as such. UIDs are always the same during the
#'   life cycle of a message.
#' @param write_to_disk If \code{TRUE}, writes the fetch content of each message
#'   to a text file in a local folder inside the working directory, also
#'   returning the results with \code{invisible()}. Default is \code{FALSE}.
#' @param keep_in_mem If \code{TRUE}, keeps a copy of each fetch result while
#'   the operation is being performed with \code{write_to_disk = TRUE}. Default
#'   is \code{FALSE}, and it can only be set \code{TRUE} when
#'   \code{write_to_disk = TRUE}.
#' @param retries Number of attempts to connect and execute the command. Default
#'   is \code{1}.
#' @param fetch_type A string indicating if it will be executed a body, header,
#'   text, or metadata fetch.
#' @param base64_decode If \code{TRUE}, tries to guess and decode the fetched
#'   text from base64 format to \code{character}. Default is \code{FALSE}. Only
#'   used in the \code{fetch_text()} case.
#' @param metadata_attribute String containing the meta attributes if applicable. Default
#'   is \code{NULL}.
#' @noRd
execute_fetch_loop <- function(self, msg_id, fetch_request, use_uid, write_to_disk,
                               keep_in_mem, retries, fetch_type, base64_decode = FALSE,
                               metadata_attribute = NULL) {

  # base64_decode is only used for fetch_text_int

  assertthat::assert_that(
    !is.na(self$con_params$folder),
    msg='No folder previously selected.')

  retries <- as.integer(retries)
  url <- self$con_params$url
  h <- self$con_handle

  msg_list <- list()
  idx = 0

  for (id in msg_id) {
    idx = idx + 1

    # the id slot is the first "#" of the request template; sub() (not gsub)
    # so that a literal "#" later in the request (e.g. in a header-field
    # name) is never touched
    adjusted_fetch_request <- sub(pattern = "#", replacement = id,
                                  x = fetch_request, fixed = TRUE)

    fetch_once <- function() {
      tryCatch({
        curl::handle_setopt(handle = h, customrequest = adjusted_fetch_request)
      }, error = function(e) {
        stop("The connection handle is dead. Please, configure a new IMAP connection with configure_imap().")
      })
      curl::curl_fetch_memory(url, handle = h)
    }

    too_large <- FALSE
    response <- tryCatch({
      fetch_once()
    }, error = function(e) {
      if (grepl("grew larger than allowed", e$message[1], fixed = TRUE)) {
        # libcurl >= 8.x (CURLE_TOO_LARGE): the literal exceeds libcurl's
        # internal buffer; re-fetch the part in partial slices below
        too_large <<- TRUE
        NULL
      } else {
        response_error_handling(e$message[1], self) # returns NULL for operation timeout: try reconnection
      }
    })

    chunked_text <- NULL
    if (is.null(response) && isTRUE(too_large)) {
      chunked_text <- fetch_in_chunks(self, adjusted_fetch_request,
                                      metadata_attribute)
      if (!is.null(chunked_text)) {
        response <- list(chunked = TRUE) # non-NULL: proceed as a normal fetch
      }
    }

    # retry path: recover the folder selection, then re-issue the same fetch;
    # the response then flows through the very same processing below, so the
    # two paths cannot drift apart
    if (is.null(response) && !isTRUE(too_large)) {
      count_retries = 0 # the first try was already counted
      select_folder_int(self, name = self$con_params$folder, mute = TRUE, retries = 0)

      while (is.null(response) && count_retries < retries) {
        count_retries = count_retries + 1
        response <- tryCatch({
          fetch_once()
        }, error = function(e) {
          response_error_handling(e$message[1], self)
        })
      }
    }

    if (is.null(response)) {
      if (isTRUE(too_large)) {
        stop('Fetch error: the server response is larger than libcurl allows in a single FETCH, and fetching it in partial slices also failed.')
      }
      stop('Fetch error: the server returned an error. Try to increase "timeout_ms".')
    }

    if (identical(id, "$")) {
      # SEARCHRES (RFC 5182): one FETCH on the saved result returns every
      # matching message in a single reply; split it into one element per
      # message and return, since "$" is necessarily the only id
      msg_list <- split_fetch_responses(rawToChar(response$headers), fetch_type,
                                        use_uid = use_uid,
                                        metadata_attribute = metadata_attribute)
      if (isTRUE(base64_decode)) {
        msg_list <- lapply(msg_list, decode_base64_text_if_needed)
      }
      return(msg_list)
    }

    if (!is.null(chunked_text)) {
      msg_text <- chunked_text
    } else {
      msg_text <- clean_fetch_results(
        rawToChar(response$headers),
        metadata_attribute # v0.9.2
      )
    }

    if (isTRUE(base64_decode)) {
      msg_list[[idx]] <- decode_base64_text_if_needed(msg_text)
    } else {
      msg_list[[idx]] <- msg_text
    }

    if (isTRUE(use_uid)) {
      names(msg_list)[idx] <- paste0(fetch_type, "UID", id) # v0.0.9
    } else {
      names(msg_list)[idx] <- paste0(fetch_type, id) # v0.0.9
    }

    if (isTRUE(write_to_disk)) {
      forbiden_chars <- "[\\/:*?\"<>|]"
      folder_clean = gsub("%20", "_", self$con_params$folder)
      folder_clean = gsub(forbiden_chars, "", folder_clean)
      user_folder = gsub(forbiden_chars, "", self$con_params$username)

      complete_path <- paste0("./", user_folder, "/", folder_clean)
      dir.create(path = complete_path, showWarnings = FALSE, recursive = TRUE)

      write(unlist(msg_list[[idx]]), paste0(complete_path, "/",
                                            names(msg_list)[idx], ".txt"))

      if (isFALSE(keep_in_mem)) { # immediately drop the content when the user does not want to keep it in memory while saving to disk
        msg_list[[idx]] <- NA
      }
    }
  } #for

  return(msg_list)

}
