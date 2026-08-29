#' Execution engine for simple commands that need no folder selection
#'
#' Issues one \code{customrequest} on the shared handle, with the same
#' error-classification and retry logic as the other engines, and returns the
#' raw server response text (headers and content pasted together, since
#' libcurl may deliver untagged lines through either callback).
#' @param self The R6 connection object.
#' @param customrequest A string with the IMAP command to issue.
#' @param retries Number of attempts to connect and execute the command.
#' @return A \code{character} string with the server response.
#' @noRd
execute_simple_command <- function(self, customrequest, retries) {

  retries <- as.integer(retries)
  url <- self$con_params$url
  h <- self$con_handle

  tryCatch({
    curl::handle_setopt(h, customrequest = customrequest)
  }, error = function(e){
    stop("The connection handle is dead. Please, configure a new IMAP connection with configure_imap().")
  })

  response <- tryCatch({
    curl::curl_fetch_memory(url, handle = h)
  }, error = function(e){
    response_error_handling(e$message[1], self)
  })

  if (is.null(response)) {
    count_retries = 0 #the first try doesnt count

    while (is.null(response) && count_retries < retries) {
      count_retries = count_retries + 1
      response <- tryCatch({
        curl::curl_fetch_memory(url, handle = h)
      }, error = function(e){
        response_error_handling(e$message[1], self)
      })
    }

    if (is.null(response)) {
      stop('Request error: the server returned an error.')
    }
  }

  resp_char <- paste(rawToChar(response$headers), rawToChar(response$content),
                     sep = "\r\n")
  rm(h)
  rm(response)

  if (self$con_params$verbose) {
    Sys.sleep(0.01)  # wait for the end of the client-server conversation
  }
  resp_char

}
