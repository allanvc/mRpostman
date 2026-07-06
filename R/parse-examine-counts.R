#' Parse EXISTS/RECENT counts from an EXAMINE/SELECT response
#'
#' Extracts the message counts from the untagged \code{* n EXISTS} /
#' \code{* n RECENT} responses and labels each value by its actual keyword
#' (instead of assuming a fixed \code{c("EXISTS", "RECENT")} position). This
#' makes the result order-independent and tolerant of a missing \code{RECENT}
#' response (some servers omit it, and it is removed altogether in IMAP4rev2).
#' @param headers_char A \code{character} string with the server response
#'   (typically \code{rawToChar(response$headers)}).
#' @return A named \code{numeric} vector with the available counts (\code{EXISTS}
#'   first, then \code{RECENT} when present).
#' @noRd
parse_examine_counts <- function(headers_char) {

  m <- stringr::str_match_all(headers_char, "(\\d+) (EXISTS|RECENT)")[[1]]

  if (nrow(m) == 0) {
    return(numeric(0))
  }

  out <- as.numeric(m[, 2])
  names(out) <- m[, 3]

  # if a keyword appears more than once, keep the last occurrence
  out <- out[!duplicated(names(out), fromLast = TRUE)]

  # stable ordering: EXISTS, then RECENT, then anything else
  out[order(match(names(out), c("EXISTS", "RECENT")))]

}
