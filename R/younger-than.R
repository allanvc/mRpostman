#' @inherit older_than
#' @note To be able to use this functionality, the server must support the
#'   \code{WITHIN} capability.
#' @family custom search
#' @examples
#' \dontrun{
#' # select folder & search
#' con$select_folder(name = "INBOX")
#' # search for messages containing the string "XYZ@@k-state.edu" in the
#' #   "FROM" field AND those that are YOUNGER than 3600 seconds (1 hour).
#' res <- con$search(request = AND(string(expr = "XYZ@@k-state.edu",
#'                                       where = "FROM"),
#'                                younger_than(seconds = 3600)))
#' }
#'
#' @export
#'
younger_than <- function(seconds, negate = FALSE) {

  check_args(seconds, negate)

  # setting part of the search string

  if (!isTRUE(negate)) {
    out = paste0('(YOUNGER ', seconds, ')')

  } else {
    out = paste0('(NOT (YOUNGER ', seconds, '))')

  }

  return(out)

}
