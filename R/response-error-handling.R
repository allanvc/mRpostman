#' Response error handling
#' @param error_message A \code{character vector} containing the error message
#'   of the curl request.
#' @noRd
response_error_handling <- function(error_message) {

  pattern_resolving = 'Timeout was reached: Resolving timed out'

  pattern_login = 'Login denied'

  error_check_resolving <- grepl(pattern = pattern_resolving, x = error_message)

  error_check_login <- grepl(pattern = pattern_login, x = error_message)

  if (error_check_resolving) {

    stop("Resolving timeout: check your internet connection status or try to increase
         the timeout_ms argument in ImapCon$new().")

  } else if (error_check_login) {

    stop("Login denied: the server returned an authentication error.")

  } else {

    return(NULL) # for operation timeout: try reconnection

  }

  # schema:
  # 1) "Login denied" -- finish all atempts rigth away
  # 2) "Timeout was reached: Resolving timed out... internet connection error -- Finish all attempts right away
  # 3) "Timeout was reached: Operation timed out..." -- return NULL to do a retry

}

