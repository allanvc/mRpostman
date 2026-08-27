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

#' Enable UTF8=ACCEPT on the current connection (INTERNAL HELPER)
#'
#' \code{ENABLE} is only accepted in the authenticated, not-selected state
#' (RFC 5161), and what it enables lives on the current TCP connection, which
#' libcurl may replace at any time (servers such as Gmail close the connection
#' after a rejected command). The connection epoch counted by the debug
#' callback tells whether the extension is still enabled; when it is not, the
#' selected folder is released with \code{UNSELECT}, the extension enabled,
#' and the folder selected again.
#' @return \code{TRUE} when UTF-8 search terms may be sent as is; \code{FALSE}
#'   when the server lacks \code{UNSELECT} and the folder cannot be released
#'   without expunging (the caller then falls back to \code{CHARSET UTF-8}).
#' @noRd
ensure_utf8_enabled <- function(self, caps, retries = 1) {
  epoch <- if (is.null(self$con_debug)) 0L else self$con_debug$epoch
  if (identical(self$utf8_epoch, epoch)) {
    return(TRUE)
  }
  folder <- self$con_params$folder
  if (!is.na(folder)) {
    if (!("UNSELECT" %in% caps)) {
      return(FALSE)
    }
    unselect_folder_int(self, retries)
    self$con_params$folder <- NA
  }
  enable_int(self, "UTF8=ACCEPT", retries)
  # the ENABLE may itself have triggered a reconnection: record the epoch now
  self$utf8_epoch <- if (is.null(self$con_debug)) 0L else self$con_debug$epoch
  if (!is.na(folder)) {
    select_folder_int(self, name = folder, mute = TRUE, retries = 0)
    self$con_params$folder <- folder
  }
  TRUE
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
