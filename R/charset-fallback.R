#' Alternative SEARCH requests after a BADCHARSET rejection (INTERNAL HELPER)
#'
#' When the server answers \code{NO [BADCHARSET ...]}, the search term must be
#' sent in a character set the server accepts. This helper rewrites the
#' \code{SEARCH} request for every candidate charset in which the term is
#' representable: the charsets the server listed in the response code (RFC
#' 3501, section 7.1), if any, followed by \code{ISO-8859-1} and
#' \code{US-ASCII}.
#' @param customrequest The rejected request (UTF-8).
#' @param reply The server's \code{NO [BADCHARSET ...]} line.
#' @return A \code{character} vector of alternative requests, possibly empty.
#' @noRd
charset_fallback_requests <- function(customrequest, reply) {
  listed <- stringr::str_match(reply, "BADCHARSET\\s*\\(([^\\)]*)\\)")[1, 2]
  listed <- if (is.na(listed)) character(0) else strsplit(trimws(listed), "\\s+")[[1]]
  current <- stringr::str_match(customrequest, "CHARSET\\s+(\\S+)")[1, 2]
  candidates <- unique(toupper(c(listed, "ISO-8859-1", "US-ASCII")))
  candidates <- setdiff(candidates, toupper(current))
  out <- character(0)
  for (cs in candidates) {
    converted <- suppressWarnings(tryCatch(iconv(customrequest, "UTF-8", cs),
                                           error = function(e) NA_character_))
    if (is.na(converted)) next
    req <- if (!is.na(current)) {
      sub("CHARSET\\s+\\S+", paste("CHARSET", cs), converted, useBytes = TRUE)
    } else {
      sub("SEARCH ", paste0("SEARCH CHARSET ", cs, " "), converted, fixed = TRUE, useBytes = TRUE)
    }
    out <- c(out, req)
  }
  unique(out)
}
