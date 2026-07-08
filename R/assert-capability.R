#' Fetch (and cache) the server's advertised IMAP capabilities (INTERNAL HELPER)
#'
#' Runs \code{CAPABILITY} once per connection and caches the result on the
#' connection object (\code{self$server_capabilities}) so repeated capability
#' checks do not incur an extra round-trip. Pass \code{refresh = TRUE} to force
#' a new \code{CAPABILITY} request.
#' @param self The \code{ImapCon} connection object.
#' @param retries Number of attempts to connect and execute the command.
#' @param refresh A \code{logical}. If \code{TRUE}, ignores the cache and
#'   re-queries the server.
#' @return A \code{character} vector of capability tokens.
#' @noRd
get_server_capabilities <- function(self, retries = 1, refresh = FALSE) {
  if (isTRUE(refresh) || is.null(self$server_capabilities)) {
    self$server_capabilities <- list_server_capabilities_int(self, retries)
  }
  self$server_capabilities
}

#' Assert the server advertises a capability required by an extension (INTERNAL HELPER)
#'
#' Most \code{ImapCon} methods map to mandatory IMAP4rev1 (RFC 3501) commands,
#' which every compliant server implements. A handful map to optional extensions
#' (SORT/THREAD, QUOTA, NAMESPACE, ID, UNSELECT, SPECIAL-USE, ...) that only work
#' when the server lists them in its \code{CAPABILITY} response. This helper
#' stops with an informative error \emph{before} issuing the command, instead of
#' letting the server reply with a cryptic \code{BAD Unknown command}.
#' @param self The \code{ImapCon} connection object.
#' @param cap The capability token to look for (case-insensitive), e.g.
#'   \code{"SORT"} or \code{"THREAD=REFERENCES"}.
#' @param command A human-readable command name for the error message.
#' @param rfc Optional RFC reference to include in the error message.
#' @param prefix A \code{logical}. If \code{TRUE}, any advertised capability that
#'   \emph{starts with} \code{cap} satisfies the check (e.g. \code{"THREAD="}
#'   matches \code{"THREAD=REFERENCES"}).
#' @param retries Number of attempts to connect and execute the command.
#' @return Invisibly \code{TRUE} if the capability is present; otherwise stops.
#' @noRd
assert_capability <- function(self, cap, command, rfc = NULL,
                              prefix = FALSE, retries = 1) {
  caps <- toupper(get_server_capabilities(self, retries = retries))
  target <- toupper(cap)
  ok <- if (isTRUE(prefix)) any(startsWith(caps, target)) else target %in% caps
  if (!isTRUE(ok)) {
    stop(sprintf(
      paste0('The IMAP server does not advertise the "%s" capability%s, ',
             'which is required by the "%s" command. This is a server ',
             'limitation, not an error in your call. Check what your server ',
             'supports with `list_server_capabilities()`.'),
      cap, if (!is.null(rfc)) paste0(" (", rfc, ")") else "", command),
      call. = FALSE)
  }
  invisible(TRUE)
}
