#' Parse a THREAD response into a list of message-id vectors
#'
#' The untagged \code{* THREAD (m)(m m)(m m (m)(m))} response groups messages
#' into threads via nested parentheses (the nesting encodes parent/child
#' relationships). This parser splits the \strong{top-level} parenthesized
#' groups and flattens each into a single \code{integer} vector of the message
#' ids belonging to that thread, in the order they appear.
#' @param content_char A \code{character} string with the server response body
#'   (typically \code{rawToChar(response$content)}).
#' @return A \code{list} of \code{integer} vectors, one per top-level thread.
#'   Returns an empty \code{list} when no THREAD response is present.
#' @noRd
parse_thread <- function(content_char) {

  m <- regmatches(content_char, regexpr("\\* THREAD .*", content_char))

  if (length(m) == 0) {
    return(list())
  }

  s <- sub("^\\* THREAD ", "", m)

  threads <- list()
  depth <- 0L
  cur <- ""
  chars <- strsplit(s, "")[[1]]

  for (ch in chars) {
    if (ch == "(") {
      if (depth == 0L) {
        cur <- ""
      } else {
        cur <- paste0(cur, ch)
      }
      depth <- depth + 1L
    } else if (ch == ")") {
      depth <- depth - 1L
      if (depth == 0L) {
        nums <- as.integer(regmatches(cur, gregexpr("\\d+", cur))[[1]])
        threads[[length(threads) + 1L]] <- nums
      } else if (depth > 0L) {
        cur <- paste0(cur, ch)
      }
    } else if (depth >= 1L) {
      cur <- paste0(cur, ch)
    }
  }

  threads

}
