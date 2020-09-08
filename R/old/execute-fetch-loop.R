#' @title Full Fetch Loop
#'
#' @description Internal helper function for loop execution used in fetch
#'     functions.
#'
#' @inheritParams check_args_fetch_full_msg
#' @param handle A curl handle object.
#'
#' @return A \code{list} or/and text files containing the fetch results.
#'
#' @family fetch helpers
#' @family loop
#'
#' @keywords internal
#'
execute_fetch_loop <- function(self, msg, fetch_request, use_uid, write, retries,
                               handle) {

  url <- self$url

  h <- handle

  # fetching
  msg_list <- list()
  idx = 0

  # loop exec
  for (id in msg) {
    idx = idx + 1

    fetch_request <- paste0(use_uid_string, "FETCH ", id, body_string, partial_string)

    tryCatch({
      curl::handle_setopt(
        handle = h,
        customrequest = fetch_request)
    }, error = function(e){
      stop("The connection handle is dead. Please, configure a new IMAP connection with ImapCon$new().")
    })

    # REQUEST
    response <- tryCatch({
      curl::curl_fetch_memory(url, handle = h)
    }, error = function(e){
      # print(e$message)
      response_error_handling(e$message[1]) # returns NULL for operation timeout: try reconnection
      id = msg[1] # return to the beggining
      idx = 0
    })

    if (!is.null(response)) {

      msg_list[[idx]] <- clean_fetch_results(
        rawToChar(response$headers))

      rm(response)

      if (isTRUE(use_uid)) {
        names(msg_list)[idx] <- paste0("msgUID", id)

      } else {
        names(msg_list)[idx] <- paste0("msg", id) # v0.0.9

      }

      if (isTRUE(write)) {

        folder_clean = gsub("%20", "_", self$folder)

        forbiden_chars <- "[\\/:*?\"<>|]"
        folder_clean = gsub(forbiden_chars, "", folder_clean)

        complete_path <- paste0("./", folder_clean)
        dir.create(path = complete_path, showWarnings = FALSE, recursive = TRUE)

        write(unlist(msg_list[[idx]]), paste0(complete_path, "/",
                                              names(msg_list)[idx], ".txt"))

        # if (isFALSE(keep_in_mem)) { # v0..0.9
        #   msg_list[[id]] <- NA
        # }

      }

    } else {
      count_retries = 0 #the first try was already counted
      # FORCE appending fresh_connect
      # curl::handle_setopt(handle = h, fresh_connect = TRUE)
      select_folder_int(self, name = self$folder, silent = TRUE, retries = 0) # ok! v0.0.9

      while (is.null(response) && count_retries < retries) {
        count_retries = count_retries + 1

        # REQUEST
        response <- tryCatch({
          curl::curl_fetch_memory(url, handle = h)
        }, error = function(e){
          # print(e$message)
          response_error_handling(e$message[1]) # returns NULL for operation timeout: try reconnection
          id = msg[1] # return to the beggining
          idx = 0
        })

        if (!is.null(response)) {

          msg_list[[idx]] <- clean_fetch_results(
            rawToChar(response$headers))

          rm(response)

          if (isTRUE(use_uid)) {
            names(msg_list)[idx] <- paste0("msgUID", id)

          } else {
            names(msg_list)[idx] <- paste0("msg", id) # v0.0.9

          }

          if (isTRUE(write)) {

            folder_clean = gsub("%20", "_", self$folder)

            forbiden_chars <- "[\\/:*?\"<>|]"
            folder_clean = gsub(forbiden_chars, "", folder_clean)

            complete_path <- paste0("./", folder_clean)
            dir.create(path = complete_path, showWarnings = FALSE, recursive = TRUE)

            write(unlist(msg_list[[idx]]), paste0(complete_path, "/",
                                                  names(msg_list)[idx], ".txt"))

            # if (isFALSE(keep_in_mem)) { # v0.9.0.0
            #   msg_list[[id]] <- NA
            # }

          }
        } else {
          stop('Fetch error: the server returned an error. Try to increase "timeout_ms".')

        }
      } #while
    } #else-response
  } #for

  return(msg_list)

}
