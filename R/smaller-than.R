#' Criterion constructor function to be combined in a custom search statement
#' @param size An integer specifying the number of bytes to be used as
#'   search criterion.
#' @param negate If \code{TRUE}, negates the search and seeks for "NOT SEARCH
#'   CRITERIA". Default is \code{FALSE}.
#' @family custom search
#' @examples
#' \dontrun{
#' # select folder & search
#' con$select_folder(name = "INBOX")
#' # search for messages containing the string "XYZ@@k-state.edu" in the
#' # "FROM" field OR those that are SMALLER than 512KB.
#' res <- con$search(request = OR(string(expr = "XYZ@@k-state.edu",
#'                                       where = "FROM"),
#'                                smaller_than(size = 512000)))
#' }
#'
#' @return A search criterion of class \code{imap_search}, to be combined
#'   into a search statement (see \code{Ops.imap_search}).
#' @export
#'
smaller_than <- function(size, negate = FALSE) {

  check_args(size = size, negate = negate)

  # never let R print large numbers in scientific notation (5e+06)
  size <- format(size, scientific = FALSE, trim = TRUE)

  # setting part of the search string

  if (!isTRUE(negate)) {
    out = paste0('(SMALLER ', size, ')')

  } else {
    out = paste0('(NOT (SMALLER ', size, '))')

  }

  return(as_imap_search(out))

}
