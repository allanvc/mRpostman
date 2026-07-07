#' Parse an ID response into a named character vector
#'
#' Turns the untagged \code{* ID ("name" "value" ...)} response (RFC 2971) into
#' a named \code{character} vector (\code{value} named by \code{name}).
#' \code{* ID NIL} yields an empty (named) vector.
#' @param content_char A \code{character} string with the server response body
#'   (typically \code{rawToChar(response$content)}).
#' @return A named \code{character} vector of server id fields, possibly empty.
#' @noRd
parse_id <- function(content_char) {

  grp <- stringr::str_match(content_char, "\\* ID (\\(.*\\)|NIL)")[, 2]

  if (is.na(grp) || grp == "NIL") {
    return(stats::setNames(character(0), character(0)))
  }

  toks <- stringr::str_match_all(grp, '"([^"]*)"')[[1]][, 2]

  if (length(toks) < 2) {
    return(stats::setNames(character(0), character(0)))
  }

  idx <- seq(1, length(toks) - 1, by = 2)
  out <- toks[idx + 1]
  names(out) <- toks[idx]
  out

}
