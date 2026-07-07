#' Parse a NAMESPACE response into its three components
#'
#' The untagged \code{* NAMESPACE <personal> <other-users> <shared>} response
#' (RFC 2342) carries three components, each either \code{NIL} or a list of
#' \code{("<prefix>" "<delimiter>")} pairs. This parser splits the three
#' top-level components (respecting nested parentheses and quoting) and turns
#' each into a \code{data.frame} of \code{prefix}/\code{delimiter} (or
#' \code{NULL} when the component is \code{NIL}).
#' @param content_char A \code{character} string with the server response body
#'   (typically \code{rawToChar(response$content)}).
#' @return A named \code{list} with elements \code{personal}, \code{other_users}
#'   and \code{shared}, each a \code{data.frame} (\code{prefix},
#'   \code{delimiter}) or \code{NULL}.
#' @noRd
parse_namespace <- function(content_char) {

  empty <- list(personal = NULL, other_users = NULL, shared = NULL)

  m <- regmatches(content_char, regexpr("\\* NAMESPACE .*", content_char))
  if (length(m) == 0) {
    return(empty)
  }

  s <- trimws(sub("^\\* NAMESPACE ", "", m))

  # split into top-level items (NIL or a balanced "(...)"), respecting quotes
  items <- character(0)
  depth <- 0L
  cur <- ""
  inq <- FALSE
  for (ch in strsplit(s, "")[[1]]) {
    if (ch == '"') {
      inq <- !inq
    }
    if (!inq && ch == " " && depth == 0L) {
      if (nzchar(cur)) {
        items <- c(items, cur)
        cur <- ""
      }
    } else {
      if (!inq && ch == "(") depth <- depth + 1L
      if (!inq && ch == ")") depth <- depth - 1L
      cur <- paste0(cur, ch)
    }
  }
  if (nzchar(cur)) {
    items <- c(items, cur)
  }

  parse_one <- function(x) {
    if (is.na(x) || x == "NIL") {
      return(NULL)
    }
    pm <- stringr::str_match_all(x, '\\("([^"]*)" (?:"([^"]*)"|NIL)\\)')[[1]]
    if (nrow(pm) == 0) {
      return(NULL)
    }
    data.frame(prefix = pm[, 2], delimiter = pm[, 3], stringsAsFactors = FALSE)
  }

  out <- empty
  if (length(items) >= 1) out$personal    <- parse_one(items[1])
  if (length(items) >= 2) out$other_users <- parse_one(items[2])
  if (length(items) >= 3) out$shared      <- parse_one(items[3])

  out

}
