#' Criterion constructor function to be combined in a custom search statement
#' @param size An integer specifying the number of seconds to be used as
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
#' @export
#'
smaller_than <- function(size, negate = FALSE) {

  check_args(size, negate)

  # setting part of the search string

  if (!isTRUE(negate)) {
    out = paste0('(SMALLER ', size, ')')

  } else {
    out = paste0('(NOT (SMALLER ', size, '))')

  }

  return(out)

}
