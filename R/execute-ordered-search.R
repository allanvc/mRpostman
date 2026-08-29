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
#' @noRd
execute_ordered_search <- function(self, url, handle, customrequest, parser, retries) {
  out <- imap_exec(self, customrequest, retries = retries, needs_folder = TRUE)
  # untagged lines may arrive through the header or the body callback
  parser(out$text)
}
