#' Close the selected mail folder WITHOUT expunging (INTERNAL HELPER)
#'
#' Issues the IMAP \code{UNSELECT} command (RFC 3691): like \code{CLOSE}, but it
#' does not permanently remove \code{\\Deleted} messages. Requires the server
#' \code{UNSELECT} capability.
#' @param retries Number of attempts to connect and execute the command.
#' @noRd
unselect_folder_int <- function(self, retries) {

  if (is.na(self$con_params$folder)) {
    stop_no_folder()
  }

  check_args(retries = retries)

  # UNSELECT is an optional extension (RFC 3691) -- fail early with a clear
  # message if the server does not advertise it.
  assert_capability(self, "UNSELECT", command = "unselect_folder",
                    rfc = "RFC 3691", retries = retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  response <- imap_exec(self, customrequest = 'UNSELECT',
                        retries = retries)$response
  # sanitizing

  self$con_stale <- TRUE  # some servers drop the connection here (see append_int)
  invisible(TRUE)

}
