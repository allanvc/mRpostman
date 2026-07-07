#' Close the selected mail folder WITHOUT expunging (INTERNAL HELPER)
#'
#' Issues the IMAP \code{UNSELECT} command (RFC 3691): like \code{CLOSE}, but it
#' does not permanently remove \code{\\Deleted} messages. Requires the server
#' \code{UNSELECT} capability.
#' @param retries Number of attempts to connect and execute the command.
#' @noRd
unselect_folder_int <- function(self, retries) {

  assertthat::assert_that(
    !is.na(self$con_params$folder),
    msg='No folder previously selected.')

  check_args(retries = retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  tryCatch({
    curl::handle_setopt(h, customrequest = 'UNSELECT')
  }, error = function(e){
    stop("The connection handle is dead. Please, configure a new IMAP connection with configure_imap().")
  })

  response <- tryCatch({
    curl::curl_fetch_memory(url, handle = h)
  }, error = function(e){
    # print(e$message)
    response_error_handling(e$message[1])
  })

  if (is.null(response)) {
    count_retries = 0 #the first try doesnt count

    while (is.null(response) && count_retries < retries) {
      count_retries = count_retries + 1
      response <- tryCatch({
        curl::curl_fetch_memory(url, handle = h)
      }, error = function(e){
        # print(e$message)
        response_error_handling(e$message[1])
      })
    }

    if (is.null(response)) {
      stop('Request error: the server returned an error.')
    }
  }

  # sanitizing
  rm(h)

  if (self$con_params$verbose) {
    Sys.sleep(0.01)  # wait for the end of the client-server conversation
  }
  invisible(TRUE)

}
