#' Decide how a non-ASCII search term is declared to the server (INTERNAL HELPER)
#'
#' Servers differ on non-ASCII search terms. Those that implement RFC 6855
#' (\code{UTF8=ACCEPT}; Gmail among them) accept UTF-8 in quoted strings only
#' after the client has enabled the extension, and reject a \code{CHARSET}
#' clause. Servers without it (Dovecot among them) need \code{CHARSET UTF-8}.
#' This helper enables \code{UTF8=ACCEPT} once per session when the server
#' advertises it and returns the \code{CHARSET} clause to prepend to the
#' request: \code{""} when nothing is needed, \code{"CHARSET UTF-8 "}
#' otherwise.
#' @param self The R6 connection object.
#' @param text The search text (request or term) that will be sent.
#' @param retries Number of attempts to connect and execute the command.
#' @noRd
search_charset_policy <- function(self, text, retries = 1) {
  text <- enc2utf8(text)
  if (!any(as.integer(charToRaw(text)) > 127L)) {
    return("")
  }
  caps <- toupper(get_server_capabilities(self, retries = retries))
  if ("UTF8=ACCEPT" %in% caps) {
    if (isTRUE(ensure_utf8_enabled(self, caps, retries))) {
      return("")
    }
  }
  "CHARSET UTF-8 "
}

ensure_utf8_enabled <- function(self, caps, retries = 1) {
  ensure_enabled(self, "UTF8=ACCEPT", caps, retries)
}

#' Verify the extensions a custom search request relies on (INTERNAL HELPER)
#'
#' The search keys of the \code{WITHIN} (RFC 5032), \code{SAVEDATE} (RFC
#' 8514), and \code{CONDSTORE} (RFC 7162) extensions are only understood by
#' servers that advertise them; without this check such a request fails with
#' an opaque \code{BAD Could not parse command}.
#' @noRd
assert_search_extensions <- function(self, request, retries = 1) {
  up <- toupper(request)
  if (grepl("\\b(YOUNGER|OLDER)\\s", up)) {
    assert_capability(self, "WITHIN", command = "search (YOUNGER/OLDER criteria)",
                      rfc = "RFC 5032", retries = retries)
  }
  if (grepl("\\bSAVED(BEFORE|ON|SINCE)\\s", up)) {
    assert_capability(self, "SAVEDATE", command = "search (SAVEDBEFORE/SAVEDON/SAVEDSINCE criteria)",
                      rfc = "RFC 8514", retries = retries)
  }
  if (grepl("\\bMODSEQ\\s", up)) {
    assert_capability(self, "CONDSTORE", command = "search (MODSEQ criterion)",
                      rfc = "RFC 7162", retries = retries)
  }
  invisible(TRUE)
}
