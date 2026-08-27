#' Response error handling
#' @param error_message A \code{character vector} containing the error message
#'   of the curl request.
#' @noRd
response_error_handling <- function(error_message) {

  pattern_resolving = 'Timeout was reached: Resolving timed out'

  pattern_login = 'Login denied'

  error_check_resolving <- grepl(pattern = pattern_resolving, x = error_message)

  error_check_login <- grepl(pattern = pattern_login, x = error_message)

  # a tagged NO/BAD reply recorded by the debug callback: the server rejected
  # the command, so retrying is pointless and the reason is worth reporting
  server_error <- last_server_error()

  if (error_check_resolving) {

    stop("Resolving timeout: check your internet connection status or try to increase
         the timeout_ms argument in ImapCon$new().")

  } else if (error_check_login) {

    stop("Login denied: the server returned an authentication error.")

  } else if (!is.na(server_error) &&
             grepl("No mailbox selected|not allowed now|not allowed in this state",
                   server_error, ignore.case = TRUE)) {

    # the folder selection was lost (e.g. after a failed SELECT or a server
    # reconnection): let the caller re-select the folder and retry
    return(NULL)

  } else if (!is.na(server_error)) {

    stop(paste0("The server rejected the command: ", server_error), call. = FALSE)

  } else if (grepl("grew larger than allowed", error_message)) {

    # libcurl >= 8.7 (CURLE_TOO_LARGE): one response line exceeded what
    # libcurl accepts, typically the id list of a SEARCH matching many
    # thousands of messages; retrying cannot help, but ESEARCH can
    stop(paste0("The response is larger than libcurl accepts in one line ",
                "(typically a SEARCH matching many thousands of messages). ",
                "Use esearch = TRUE, which condenses the id list, or an ",
                "esearch_*() aggregation, or restrict the search criteria."),
         call. = FALSE)

  } else {

    return(NULL) # for operation timeout: try reconnection

  }

  # schema:
  # 1) "Login denied" -- finish all atempts rigth away
  # 2) "Timeout was reached: Resolving timed out... internet connection error -- Finish all attempts right away
  # 3) "Timeout was reached: Operation timed out..." -- return NULL to do a retry

}

