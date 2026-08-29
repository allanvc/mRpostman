#' Close the selected mail folder, expunging \\Deleted messages (INTERNAL HELPER)
#' @param retries Number of attempts to connect and execute the command.
#' @noRd
close_folder_int <- function(self, retries) {

  if (is.na(self$con_params$folder)) {
    stop_no_folder()
  }

  check_args(retries = retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  tryCatch({
    curl::handle_setopt(h, customrequest = 'CLOSE')
  }, error = function(e){
    stop("The connection handle is dead. Please, configure a new IMAP connection with configure_imap().")
  })

  response <- tryCatch({
    curl::curl_fetch_memory(url, handle = h)
  }, error = function(e){
    # print(e$message)
    response_error_handling(e$message[1], self)
  })

  if (is.null(response)) {
    count_retries = 0 #the first try doesnt count

    while (is.null(response) && count_retries < retries) {
      count_retries = count_retries + 1
      response <- tryCatch({
        curl::curl_fetch_memory(url, handle = h)
      }, error = function(e){
        # print(e$message)
        response_error_handling(e$message[1], self)
      })
    }

    if (is.null(response)) {
      stop('Request error: the server returned an error.')
    }
  }

  # sanitizing

  self$con_stale <- TRUE  # some servers drop the connection here (see append_int)
  invisible(TRUE)

}
