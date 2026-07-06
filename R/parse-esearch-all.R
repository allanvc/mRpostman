#' Expand the ALL sequence-set of an ESEARCH response into message ids
#'
#' Parses the \code{ALL <sequence-set>} item of an \code{ESEARCH} response
#' (e.g. \code{* ESEARCH (TAG "A2") ALL 1:3,5,9:11}) and expands the IMAP
#' sequence-set into an integer vector. This replaces the previous
#' \code{eval(parse(text = paste0("c(", ...), ")")))} approach, which evaluated
#' server-provided text as R code (a robustness and code-injection risk).
#' @param content_char A \code{character} string with the raw response
#'   (typically \code{rawToChar(response$content)}).
#' @return An \code{integer} vector with the expanded message ids
#'   (\code{integer(0)} when there is no \code{ALL} result).
#' @noRd
parse_esearch_all <- function(content_char) {

  m <- stringr::str_match_all(content_char, "ALL ([0-9,:]+)")[[1]]

  if (nrow(m) == 0) {
    return(integer(0))
  }

  ids <- integer(0)
  for (seqset in m[, 2]) {
    for (tok in strsplit(seqset, ",", fixed = TRUE)[[1]]) {
      if (grepl(":", tok, fixed = TRUE)) {
        b <- suppressWarnings(as.integer(strsplit(tok, ":", fixed = TRUE)[[1]]))
        if (length(b) == 2 && !anyNA(b)) {
          ids <- c(ids, seq.int(b[1], b[2]))
        }
      } else {
        n <- suppressWarnings(as.integer(tok))
        if (!is.na(n)) {
          ids <- c(ids, n)
        }
      }
    }
  }

  ids

}
