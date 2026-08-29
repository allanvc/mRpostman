#' Get the quota of a quota root (INTERNAL HELPER)
#' @param quota_root A string with the quota root name (often \code{""} for the
#'   default root).
#' @param retries Number of attempts to connect and execute the command.
#' @noRd
get_quota_int <- function(self, quota_root, retries) {

  assertthat::assert_that(
    is.character(quota_root),
    msg='"quota_root" must be of type character.')

  check_args(retries = retries)

  # QUOTA is an optional extension (RFC 9208) -- fail early with a clear
  # message if the server does not advertise it.
  assert_capability(self, "QUOTA", command = "get_quota",
                    rfc = "RFC 9208", retries = retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  root <- adjust_folder_name(quota_root) # quotes it ("" -> "\"\"")

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  response <- imap_exec(self, customrequest = paste0("GETQUOTA ", root),
                        retries = retries)$response
  resp_char <- paste(rawToChar(response$headers),
                     rawToChar(response$content), sep = "\r\n")
  final_output <- parse_quota(resp_char)

  # sanitizing

  return(final_output)

}
