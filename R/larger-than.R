#' @inherit smaller_than
#' @family custom search
#' @examples
#' \dontrun{
#' # select folder & search
#' con$select_folder(name = "INBOX")
#' # search for messages containing the string "XYZ@@k-state.edu" in the
#' #   "FROM" field OR those that are LARGER than 512KB.
#' res <- con$search(request = OR(string(expr = "XYZ@@k-state.edu",
#'                                       where = "FROM"),
#'                                larger_than(size = 512000)))
#' }
#'
#' @export
#'
larger_than <- function(size, negate = FALSE) {

  check_args(size, negate)

  # setting part of the search string

  if (!isTRUE(negate)) {
    out = paste0('(LARGER ', size, ')')

  } else {
    out = paste0('(NOT (LARGER ', size, '))')

  }

  return(out)

}
