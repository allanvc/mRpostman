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
#'   @noRd
execute_fetch_loop <- function(self, msg_id, fetch_request, use_uid, write_to_disk,
                               keep_in_mem, retries, fetch_type, base64_decode = FALSE) {

  # base64_decode is only used for fetch_text_int

  # previous folder selection checking
  # if (is.na(self$folder)) {
  #   stop('No folder previously selected.')
  # }
  assertthat::assert_that(
    !is.na(self$con_params$folder),
    msg='No folder previously selected.')

  # forcing retries as an integer
  retries <- as.integer(retries)

  url <- self$con_params$url

  h <- self$con_handle

  # fetching
  msg_list <- list()
  idx = 0

  # loop exec
  for (id in msg_id) {
    idx = idx + 1

    adjusted_fetch_request <- gsub(pattern = "#", replacement = id, x = fetch_request)

    tryCatch({
      curl::handle_setopt(
        handle = h,
        customrequest = adjusted_fetch_request)
    }, error = function(e){
      stop("The connection handle is dead. Please, configure a new IMAP connection with ImapCon$new().")
    })

    # REQUEST
    response <- tryCatch({
      curl::curl_fetch_memory(url, handle = h)
    }, error = function(e){
      # print(e$message)
      response_error_handling(e$message[1]) # returns NULL for operation timeout: try reconnection
      # id = msg_id[1] # return to the beginning
      # idx = 0
    })

    # print(exists("response")); print(exists("response")); print(exists("response"))

    if (!is.null(response)) {
      if (isTRUE(base64_decode)) {
        msg_list[[idx]] <- decode_base64_text_if_needed(
          clean_fetch_results(
            rawToChar(response$headers)
          )
        )

      } else {
        msg_list[[idx]] <- clean_fetch_results(
          rawToChar(response$headers)
        )

      }

    # if (!is.null(response)) {
    #
    #   msg_list[[idx]] <- clean_fetch_results(
    #     rawToChar(response$headers))
    #
    #   rm(response)

      if (isTRUE(use_uid)) {
        names(msg_list)[idx] <- paste0(fetch_type, "UID", id) # v0.0.9

      } else {
        names(msg_list)[idx] <- paste0(fetch_type, id) # v0.0.9

      }

      if (isTRUE(write_to_disk)) {

        folder_clean = gsub("%20", "_", self$con_params$folder)

        forbiden_chars <- "[\\/:*?\"<>|]"
        folder_clean = gsub(forbiden_chars, "", folder_clean)

        # url <- "imaps://outlook.office365.com/"
        url_folder <- unlist(regmatches(url, gregexpr("://(.*?)(/|.)$", url)))
        url_folder = gsub(forbiden_chars, "", url_folder)

        complete_path <- paste0("./", url_folder, "/", folder_clean)
        dir.create(path = complete_path, showWarnings = FALSE, recursive = TRUE)

        write(unlist(msg_list[[idx]]), paste0(complete_path, "/",
                                              names(msg_list)[idx], ".txt"))

        if (isFALSE(keep_in_mem)) { # immediately delete the content in case the user does not want to keep in memory while saving to disk
          msg_list[[id]] <- NA
        }

      }

    } else {
      count_retries = 0 #the first try was already counted
      # FORCE appending fresh_connect
      # curl::handle_setopt(handle = h, fresh_connect = TRUE)
      select_folder_int(self, name = self$con_params$folder, mute = TRUE, retries = 0) # ok! v0.0.9

      while (is.null(response) && count_retries < retries) {
        count_retries = count_retries + 1

        # reset customrequest in handle
        tryCatch({
          curl::handle_setopt(
            handle = h,
            customrequest = adjusted_fetch_request) #bug: response was NULL when recovering from a fetch timeout error
        }, error = function(e){
          stop("The connection handle is dead. Please, configure a new IMAP connection with configure_imap().")
        })

        # REQUEST
        response <- tryCatch({
          curl::curl_fetch_memory(url, handle = h)
        }, error = function(e){
          # print(e$message)
          response_error_handling(e$message[1]) # returns NULL for operation timeout: try reconnection
          # id = msg_id[1] # return to the beginning
          # idx = 0
        })

        if (!is.null(response)) {

          msg_list[[idx]] <- clean_fetch_results(
            rawToChar(response$headers))

          # rm(response)

          if (isTRUE(use_uid)) {
            names(msg_list)[idx] <- paste0(fetch_type, "UID", id)

          } else {
            names(msg_list)[idx] <- paste0(fetch_type, id) # v0.0.9

          }

          if (isTRUE(write_to_disk)) {

            folder_clean = gsub("%20", "_", self$con_params$folder)

            forbiden_chars <- "[\\/:*?\"<>|]"
            folder_clean = gsub(forbiden_chars, "", folder_clean)

            # url <- "imaps://outlook.office365.com/"
            url_folder <- regmatches(url, gregexpr("://(.*?)(/|.)$", url))
            url_folder = gsub(forbiden_chars, "", url_folder)

            complete_path <- paste0("./", url_folder, "/", folder_clean)
            dir.create(path = complete_path, showWarnings = FALSE, recursive = TRUE)

            write(unlist(msg_list[[idx]]), paste0(complete_path, "/",
                                                  names(msg_list)[idx], ".txt"))

            if (isFALSE(keep_in_mem)) { # immediately delete the content in case the user does not want to keep in memory while saving to disk
              msg_list[[id]] <- NA
            }

          }
        } else {
          stop('Fetch error: the server returned an error. Try to increase "timeout_ms".')

        }
      } #while
    } #else-response
  } #for

  return(msg_list)

}
