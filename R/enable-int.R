#' Enable server extensions for the session (INTERNAL HELPER)
#'
#' Issues \code{ENABLE <capability> ...} (RFC 5161) and returns the
#' extensions the server confirmed in its untagged \code{* ENABLED} response.
#' @param capabilities A character vector of capability names to enable.
#' @param retries Number of attempts to connect and execute the command.
#' @noRd
enable_int <- function(self, capabilities, retries) {
  assertthat::assert_that(is.character(capabilities), length(capabilities) >= 1,
                          msg='"capabilities" must be a character vector.')
  check_args(retries = retries)
  assert_capability(self, "ENABLE", command = "enable", rfc = "RFC 5161",
                    retries = retries)
  resp_char <- execute_simple_command(
    self, paste("ENABLE", paste(toupper(capabilities), collapse = " ")), retries)
  parse_enabled(resp_char)
}

#' Parse an ENABLED response (RFC 5161)
#' @param resp_char A \code{character} string with the server response.
#' @return A \code{character} vector with the enabled capabilities (possibly
#'   empty).
#' @noRd
parse_enabled <- function(resp_char) {
  m <- stringr::str_match(resp_char, "\\*\\s+ENABLED([^\r\n]*)")
  if (is.na(m[1, 2])) return(character(0))
  toks <- strsplit(trimws(m[1, 2]), "\\s+")[[1]]
  toks[nzchar(toks)]
}
