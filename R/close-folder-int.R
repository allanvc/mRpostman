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

  response <- imap_exec(self, customrequest = 'CLOSE',
                        retries = retries)$response
  # sanitizing

  self$con_stale <- TRUE  # some servers drop the connection here (see append_int)
  invisible(TRUE)

}
