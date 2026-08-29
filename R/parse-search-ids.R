#' Extract the message ids of a plain SEARCH response
#'
#' Reads the ids of the untagged \code{* SEARCH n n n ...} line. The
#' \code{(MODSEQ n)} item that servers append when the search uses the
#' CONDSTORE \code{MODSEQ} criterion (RFC 7162) is discarded, so that the
#' modification sequence is never mistaken for a message id.
#' @param content_char A \code{character} string with the server response.
#' @return A \code{numeric} vector of ids (possibly empty).
#' @noRd
parse_search_ids <- function(content_char) {
  m <- stringr::str_match_all(content_char, "\\*\\s+SEARCH([^\r\n]*)")[[1]]
  if (nrow(m) == 0) {
    return(numeric(0))
  }
  body <- gsub("\\(MODSEQ\\s+\\d+\\)", "", paste(m[, 2], collapse = " "))
  as.numeric(stringr::str_extract_all(body, "\\d+")[[1]])
}
