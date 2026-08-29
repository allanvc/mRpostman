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
  imap_exec(self, customrequest, retries = retries)$text
}
