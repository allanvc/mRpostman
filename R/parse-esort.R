#' Parse an ESEARCH-style response of SORT RETURN (...) (ESORT, RFC 5267)
#'
#' Reads the \code{* ESEARCH (TAG "x") [UID] COUNT n MIN n MAX n ALL set}
#' response into a named list.
#' @param content_char A \code{character} string with the server response.
#' @return A \code{list} whose elements are the returned items: \code{count},
#'   \code{min}, \code{max} (integers) and \code{all} (an integer vector in
#'   server order).
#' @noRd
parse_esort <- function(content_char) {
  m <- stringr::str_match(content_char, "\\*\\s+ESEARCH([^\r\n]*)")
  out <- list()
  if (is.na(m[1, 2])) return(out)
  body <- m[1, 2]
  for (key in c("COUNT", "MIN", "MAX")) {
    v <- stringr::str_match(body, paste0("\\b", key, "\\s+(\\d+)"))[1, 2]
    if (!is.na(v)) out[[tolower(key)]] <- as.integer(v)
  }
  all <- stringr::str_match(body, "\\bALL\\s+([0-9,:]+)")[1, 2]
  if (!is.na(all)) {
    # ESORT "ALL" keeps the sort order: ranges may be descending (e.g. 9:5)
    ids <- integer(0)
    for (tok in strsplit(all, ",", fixed = TRUE)[[1]]) {
      if (grepl(":", tok, fixed = TRUE)) {
        b <- as.integer(strsplit(tok, ":", fixed = TRUE)[[1]])
        ids <- c(ids, seq.int(b[1], b[2]))
      } else {
        ids <- c(ids, as.integer(tok))
      }
    }
    out$all <- ids
  }
  out
}
