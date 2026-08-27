#' Parse the status data items from a STATUS response
#'
#' Extracts the \code{(KEY value KEY value ...)} pairs from the untagged
#' \code{* STATUS <folder> (...)} response and labels each value by its actual
#' keyword. This keeps the result order-independent and tolerant of servers
#' that return the requested items in a different order.
#' @param resp_char A \code{character} string with the server response
#'   (typically \code{rawToChar(response$headers)} pasted with
#'   \code{rawToChar(response$content)}).
#' @return A named \code{numeric} vector with the returned status counts, in the
#'   order they appear in the response. Returns \code{numeric(0)} when no STATUS
#'   list is present.
#' @noRd
parse_status_counts <- function(resp_char) {

  # isolate the "(...)" data-item list of the untagged "* STATUS" response.
  # We anchor on the leading "* STATUS" (asterisk + whitespace) so we do not
  # accidentally match the "STATUS" inside the "LIST-STATUS" capability token,
  # which is present in the buffer after a reconnection and would otherwise
  # capture the "(Success)" of the "... authenticated (Success)" line instead.
  grp <- stringr::str_match(resp_char, "\\*[[:space:]]*STATUS[^\\(]*\\(((?:[^()]|\\([^()]*\\))*)\\)")[, 2]

  if (is.na(grp)) {
    return(numeric(0))
  }

  # values are numbers, except MAILBOXID (OBJECTID, RFC 8474), whose value is
  # an object identifier in parentheses
  pairs <- stringr::str_match_all(grp, "([A-Za-z]+)\\s+(\\d+|\\(([^)]*)\\))")[[1]]

  if (nrow(pairs) == 0) {
    return(numeric(0))
  }

  vals <- ifelse(is.na(pairs[, 4]), pairs[, 3], pairs[, 4])
  out <- suppressWarnings(as.numeric(vals))
  if (anyNA(out)) {
    # a non-numeric item is present: return everything as character
    out <- vals
  }
  names(out) <- toupper(pairs[, 2])

  # if a keyword appears more than once, keep the last occurrence
  out[!duplicated(names(out), fromLast = TRUE)]

}
