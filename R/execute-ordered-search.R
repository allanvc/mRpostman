#' Execution engine for the SORT and THREAD commands
#'
#' Mirrors \code{execute_search()} (same request / folder-reselect / retry
#' logic) but delegates response parsing to \code{parser} and, crucially, does
#' \strong{not} apply \code{fix_search_stripping()} — SORT/THREAD responses are
#' ordered/grouped by the server and must not be re-sorted.
#' @param self The R6 connection object.
#' @param url A string containing the connection url.
#' @param handle A curl handle with the custom request already defined.
#' @param customrequest A string with the custom request (used to reset the
#'   handle on retry).
#' @param parser A function applied to \code{rawToChar(response$content)} that
#'   returns the parsed result (e.g. \code{parse_sort} or \code{parse_thread}).
#' @param retries Number of attempts to connect and execute the command.
#' @param auth The connection credentials (\code{private$auth} of the R6
#'   object), used only when the response is too large for libcurl; see below.
#' @details SORT and THREAD answer in a single untagged line carrying every
#'   matching id, and neither has an ESEARCH-style condensed form. In a folder
#'   with tens of thousands of messages that line exceeds libcurl's response
#'   limit (CURLE_TOO_LARGE). When that happens and the credentials are
#'   available, the very same command is re-issued once on the package's
#'   raw-socket session (the one \code{idle()} and \code{append_msgs()} use),
#'   which reads lines of any length, and its reply is parsed in the same way.
#' @noRd
execute_ordered_search <- function(self, url, handle, customrequest, parser, retries,
                                   auth = NULL) {
  out <- tryCatch(
    imap_exec(self, customrequest, retries = retries, needs_folder = TRUE),
    mRpostman_response_too_large = function(e) {
      if (is.null(auth)) stop(e)
      NULL
    })
  if (is.null(out)) {
    return(parser(ordered_search_raw(self, auth, customrequest)))
  }
  # untagged lines may arrive through the header or the body callback
  parser(out$text)
}

# Run one SORT/THREAD command on the raw-socket session and return the
# untagged lines of the reply as a single string (the parsers' input).
ordered_search_raw <- function(self, auth, customrequest) {
  timeout_ms <- max(30000, self$con_params$timeout_ms)
  sess <- raw_session_start(self, auth, timeout_ms = timeout_ms)
  on.exit(raw_session_close(sess), add = TRUE)
  raw_select(sess, self$con_params$folder)
  # the server may need a while to thread/sort a very large folder
  r <- raw_command(sess, customrequest, timeout_ms = max(300000, timeout_ms))
  raw_ok_or_stop(r, sub("^(UID )?(SORT|THREAD).*$", "\\1\\2", customrequest))
  paste(r$lines, collapse = "\r\n")
}
