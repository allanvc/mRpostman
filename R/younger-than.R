#' @inherit older_than
#' @note To be able to use this functionality, the server must support the
#'   \code{WITHIN} capability.
#' @family custom search
#' @examples
#' \dontrun{
#' # select folder & search
#' con$select_folder(folder = "INBOX")
#' # search for messages containing the string "XYZ@@k-state.edu" in the
#' #   "FROM" field AND those that are YOUNGER than 3600 seconds (1 hour).
#' res <- con$search(request = AND(string(expr = "XYZ@@k-state.edu",
#'                                       where = "FROM"),
#'                                younger_than(seconds = 3600)))
#' }
#'
#' @return A search criterion of class \code{imap_search}, to be combined
#'   into a search statement (see \code{Ops.imap_search}).
#' @export
#'
younger_than <- function(seconds, negate = FALSE) {

  check_args(seconds = seconds, negate = negate)

  # never let R print large numbers in scientific notation (5e+06)
  seconds <- format(seconds, scientific = FALSE, trim = TRUE)

  # setting part of the search string

  if (!isTRUE(negate)) {
    out = paste0('(YOUNGER ', seconds, ')')

  } else {
    out = paste0('(NOT (YOUNGER ', seconds, '))')

  }

  return(as_imap_search(out))

}
