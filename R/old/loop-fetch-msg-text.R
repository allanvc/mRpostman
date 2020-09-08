#' @title Metadata Fetch Loop
#'
#' @inherit loop_fetch_full_msg description return
#'
#' @inheritParams check_args_fetch_msg_text
#' @param handle A curl handle object.
#'
#' @return A \code{list} or/and text files containing the fetch results.
#'
#' @family fetch helpers
#' @family loop
#'
#' @keywords internal
#'
loop_fetch_msg_text <- function(url, msg_id, by, peek, partial, write_to_disk,
                                keep_in_mem, try_b64decode, retries, handle) {

  h = handle

  # preparation

  # peek
  if (isTRUE(peek)) {
    body_string = " BODY.PEEK[TEXT]"
  } else {
    body_string = " BODY[TEXT]"
  }

  # partial
  if (!is.null(partial)) {
    partial_string = paste0("<", partial, ">")
  } else {
    partial_string = NULL
  }

  # by
  if (by == "UID") {
    by_string = "UID "
  } else {
    by_string = NULL
  }


  # fetching
  msg_list <- list()
  idx = 0

  for (id in msg_id) {
    idx = idx+1

    tryCatch({
      curl::handle_setopt(
        handle = h,
        customrequest = paste0(by_string, "FETCH ", id, body_string, partial_string))
    }, error = function(e){
      stop("The connection handle is dead. Please, configure a new IMAP connection with configure_imap().")
    })

    # REQUEST
    response <- tryCatch({
      curl::curl_fetch_memory(url, handle = h)
    }, error = function(e){
      # print(e$message)
      response_error_handling(e$message[1]) # returns NULL for operation timeout: try reconnection
    })

    if (!is.null(response)) {
      if (isTRUE(try_b64decode)) {
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


      rm(response)

      if (by == "UID") {
        names(msg_list)[idx] <- paste0("textUID", id)

      } else {
        names(msg_list)[idx] <- paste0("textMSN", id)

      }

      if (isTRUE(write_to_disk)) {

        folder_clean = gsub("%20", "_", IMAP_conn$imapconf$folder)

        forbiden_chars <- "[\\/:*?\"<>|]"
        folder_clean = gsub(forbiden_chars, "", folder_clean)

        complete_path <- paste0("./", folder_clean)
        dir.create(path = complete_path, showWarnings = FALSE, recursive = TRUE)

        write(unlist(msg_list[[idx]]), paste0(complete_path, "/",
                                              names(msg_list)[idx], ".txt"))

        if (isFALSE(keep_in_mem)) {
          msg_list[[id]] <- NA
        }

      }

    } else {
      count_retries = 1 #the first try was already counted
      # FORCE appending fresh_connect
      # curl::handle_setopt(handle = h, fresh_connect = TRUE)

      while (is.null(response) && count_retries < retries) {
        count_retries = count_retries+1

        # REQUEST
        response <- tryCatch({
          curl::curl_fetch_memory(url, handle = h)
        }, error = function(e){
          # print(e$message)
          response_error_handling(e$message[1]) # returns NULL for operation timeout: try reconnection
        })

        if (!is.null(response)) {
          if (isTRUE(try_b64decode)) {
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

          rm(response)

          if (by == "UID") {
            names(msg_list)[idx] <- paste0("textUID", id)

          } else {
            names(msg_list)[idx] <- paste0("textMSN", id)

          }

          if (isTRUE(write_to_disk)) {

            folder_clean = gsub("%20", "_", IMAP_conn$imapconf$folder)

            forbiden_chars <- "[\\/:*?\"<>|]"
            folder_clean = gsub(forbiden_chars, "", folder_clean)

            complete_path <- paste0("./", folder_clean)
            dir.create(path = complete_path, showWarnings = FALSE, recursive = TRUE)

            write(unlist(msg_list[[idx]]), paste0(complete_path, "/",
                                                  names(msg_list)[idx], ".txt"))

            if (isFALSE(keep_in_mem)) {
              msg_list[[id]] <- NA
            }

          }
        } else {
          stop('Fetch error: the server returned an error. Try to increase "timeout_ms".')

        }
      } #while
    } #else-response
  } #for

  if (isFALSE(keep_in_mem)) {
    rm(msg_list)
    return(TRUE)

  } else {
    return(msg_list)
  }

}
