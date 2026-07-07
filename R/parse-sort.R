#' Parse a SORT response into an ordered vector of message ids
#'
#' Extracts the message ids from the untagged \code{* SORT n n n ...} response,
#' \strong{preserving the server-provided order} (SORT results are ordered by
#' the sort keys, not ascending, so they must not be re-sorted or passed through
#' \code{fix_search_stripping()}).
#' @param content_char A \code{character} string with the server response body
#'   (typically \code{rawToChar(response$content)}).
#' @return An \code{integer} vector of message ids in the server-provided order,
#'   or \code{integer(0)} when the SORT result is empty.
#' @noRd
parse_sort <- function(content_char) {

  grp <- stringr::str_match(content_char, "\\* SORT([0-9 ]*)")[, 2]

  if (is.na(grp) || !nzchar(trimws(grp))) {
    return(integer(0))
  }

  ids <- as.integer(strsplit(trimws(grp), "\\s+")[[1]])
  ids[!is.na(ids)]

}
