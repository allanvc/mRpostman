#' Request a checkpoint of the selected mail folder (INTERNAL HELPER)
#'
#' Issues the IMAP \code{CHECK} command (RFC 3501, section 6.4.1), which asks
#' the server to perform any implementation-dependent housekeeping of the
#' selected mailbox (e.g. flushing its state to disk). It has no
#' client-observable effect; \code{NOOP} is the appropriate keep-alive.
#' @param retries Number of attempts to connect and execute the command.
#' @noRd
check_int <- function(self, retries) {

  if (is.na(self$con_params$folder)) {
    stop_no_folder()
  }

  check_args(retries = retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  execute_complementary_operations(self, url, handle = h,
                                   customrequest = "CHECK", retries)

  invisible(TRUE)

}
