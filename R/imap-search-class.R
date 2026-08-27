#' Mark a string as an IMAP search criterion (INTERNAL HELPER)
#'
#' The criterion constructors (\code{string()}, \code{flag()},
#' \code{sent_since()}, ...) and the combinators return their protocol
#' string with the class \code{"imap_search"}, so that R's own boolean
#' operators can combine them (see \code{Ops.imap_search}).
#' @noRd
as_imap_search <- function(x) {
  structure(unclass(x), class = "imap_search")
}

#' Combine two criteria with an implicit AND, as one parenthesized key
#' @noRd
qand <- function(a, b) {
  as_imap_search(paste0("(", unclass(a), " ", unclass(b), ")"))
}

#' Combine two criteria with OR; the operands are already single keys
#' @noRd
qor <- function(a, b) {
  as_imap_search(paste0("(OR ", unclass(a), " ", unclass(b), ")"))
}

#' Negate a criterion
#' @noRd
qnot <- function(a) {
  as_imap_search(paste0("(NOT ", unclass(a), ")"))
}

#' Combine search criteria with R's own operators
#'
#' Criteria built with the constructor functions (\code{string()},
#' \code{flag()}, \code{before()}, \code{sent_since()},
#' \code{larger_than()}, and their relatives) can be combined with the
#' native operators \code{&} (AND), \code{|} (OR), and \code{!} (NOT),
#' as an alternative to \code{\link{AND}} and \code{\link{OR}}.
#' Precedence and grouping follow R's own rules, so parentheses work as
#' usual. See also \code{ImapCon$query()} for the expression-based
#' interface that does not require constructor calls at all.
#'
#' @param e1,e2 Search criteria built by the constructor functions.
#' @return A search criterion string of class \code{"imap_search"}, to be
#'   passed to \code{ImapCon$search()}.
#' @examples
#' \dontrun{
#' con$search(string("budget", where = "SUBJECT") &
#'              (sent_since(date_char = "01-Oct-2001") | !flag("SEEN")))
#' }
#' @export
Ops.imap_search <- function(e1, e2) {
  if (.Generic == "!") {
    return(qnot(e1))
  }
  if (.Generic == "&") {
    return(qand(e1, e2))
  }
  if (.Generic == "|") {
    return(qor(e1, e2))
  }
  stop('operator "', .Generic, '" is not defined for search criteria; ',
       "use & (AND), | (OR), and ! (NOT).", call. = FALSE)
}

#' @export
print.imap_search <- function(x, ...) {
  cat("<imap_search> ", unclass(x), "\n", sep = "")
  invisible(x)
}
