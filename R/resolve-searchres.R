#' Resolve the SEARCHRES "$" reference into message ids (INTERNAL HELPER)
#'
#' Issues \code{SEARCH RETURN (ALL) $} (ESEARCH, RFC 4731, with the saved
#' result of RFC 5182 as the only criterion) on the connection that holds the
#' saved result, and expands the condensed id list. The saved result lives on
#' the server-side connection: after a reconnection it is empty, and so is
#' the value returned here.
#' @param self The R6 connection object.
#' @param use_uid If \code{TRUE}, UIDs; otherwise sequence numbers.
#' @param retries Number of attempts.
#' @return An integer vector of ids, possibly empty.
#' @noRd
resolve_searchres <- function(self, use_uid, retries) {
  request <- paste0(if (isTRUE(use_uid)) "UID " else "", "SEARCH RETURN (ALL) $")
  r <- imap_exec(self, request, retries = retries, needs_folder = TRUE,
                 command = "SEARCH $")
  # the wire alone (`text` pastes the body callback too, which repeats the
  # untagged line on some libcurl versions)
  wire <- wire_text(r$response$headers)
  ids <- parse_esearch_all(wire)
  if (length(ids) == 0 && !grepl("\\* ESEARCH", wire)) {
    stop_server_rejected(trimws(sub("^\\S+ ", "", wire)), command = "SEARCH $")
  }
  ids
}
