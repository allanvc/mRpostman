#' Relational-operator-function to construct a custom search statement
#' @param ... a combination of criteria constructor functions with its arguments.
#' @param negate If \code{TRUE}, negates the search and seeks for
#'   "NOT search_criterion". Default is \code{FALSE}.
#' @return A search string to be used as a \code{request} parameter in
#'  \code{ImapCon$search()} function.
#' @family custom search
#' @examples
#' \dontrun{
#' # select folder & search
#' con$select_folder(name = "INBOX")
#' # search for messages SINCE "30-Ago-2019" AND SMALLER than 512KB.
#' res <- con$search(request = AND(sent_since(date_char = "30-Ago-2019"),
#'                                 smaller_than(size = 512000)))
#' }
#' @export
#'
AND <- function(..., negate = FALSE) {
  # just a wrapper to paste

  # ... must be 2+ args
  argg <- list(...)

  assertthat::assert_that(
    length(argg) >= 2
    , msg='AND() requires two or more arguments.')

  assertthat::assert_that(
    is.logical(negate),
    msg='"negate" must be a logical.')

  # setting part of the search string

  if (!isTRUE(negate)) {
    out = paste0(paste(..., sep=' ')) # we'll already have parenthesis
    #... inside '...' args

  } else {
    out = paste0('NOT (', paste(..., sep = ' '), ')')

  }

  return(out)

}
