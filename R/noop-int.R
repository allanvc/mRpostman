#' @description Issue a NOOP command (INTERNAL HELPER)
#'
#' Sends the IMAP \code{NOOP} command. It does nothing on the server other than
#' resetting the inactivity autologout timer, which makes it useful as a
#' keep-alive and as a way to solicit pending untagged status updates.
#' @param retries Number of attempts to connect and execute the command.
#'   Default is \code{1}.
#' @noRd
noop_int <- function(self, retries) {

  check_args(retries = retries) # we have to pass
  #.. the argg as arg = arg, in order to the check_argg capture the names

  # forcing retries as an integer
  retries <- as.integer(retries)

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  response <- imap_exec(self, customrequest = 'NOOP',
                        retries = retries)$response
  # sanitizing

  invisible(TRUE)

}
