#' @inherit smaller_than
#' @family custom search
#' @examples
#' \dontrun{
#' # select folder & search
#' con$select_folder(folder = "INBOX")
#' # search for messages containing the string "XYZ@@k-state.edu" in the
#' #   "FROM" field OR those that are LARGER than 512KB.
#' res <- con$search(request = OR(string(expr = "XYZ@@k-state.edu",
#'                                       where = "FROM"),
#'                                larger_than(size = 512000)))
#' }
#'
#' @return A search criterion of class \code{imap_search}, to be combined
#'   into a search statement (see \code{Ops.imap_search}).
#' @export
#'
larger_than <- function(size, negate = FALSE) {

  check_args(size = size, negate = negate)

  # never let R print large numbers in scientific notation (5e+06)
  size <- format(size, scientific = FALSE, trim = TRUE)

  # setting part of the search string

  if (!isTRUE(negate)) {
    out = paste0('(LARGER ', size, ')')

  } else {
    out = paste0('(NOT (LARGER ', size, '))')

  }

  return(as_imap_search(out))

}
