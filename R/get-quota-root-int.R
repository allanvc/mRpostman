#' Get the quota root(s) and quota of a mail folder (INTERNAL HELPER)
#' @param name A string with the mail folder name. If \code{NULL}, uses the
#'   previously selected folder.
#' @param retries Number of attempts to connect and execute the command.
#' @noRd
get_quota_root_int <- function(self, name, retries) {

  if (!is.null(name)) {
    assertthat::assert_that(
      is.character(name),
      msg='"name" must be of type character or NULL.')
  } else {
    assertthat::assert_that(
      !is.na(self$con_params$folder),
      msg='No folder previously selected.')
  }

  check_args(retries = retries)

  # QUOTA is an optional extension (RFC 2087) -- fail early with a clear
  # message if the server does not advertise it.
  assert_capability(self, "QUOTA", command = "get_quota_root",
                    rfc = "RFC 2087", retries = retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  folder <- if (is.null(name)) {
    adjust_folder_name(self$con_params$folder)
  } else {
    adjust_folder_name(name)
  }

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  response <- imap_exec(self, customrequest = paste0("GETQUOTAROOT ", folder),
                        retries = retries)$response
  # the untagged * QUOTA line may arrive via headers or content
  resp_char <- paste(rawToChar(response$headers),
                     rawToChar(response$content), sep = "\r\n")
  final_output <- parse_quota(resp_char)

  # sanitizing

  return(final_output)

}
