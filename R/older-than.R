#' Criterion constructor function to be combined in a custom search statement
#' @param seconds An integer specifying the number of seconds to be used as
#'   the search criterion.
#' @param negate If \code{TRUE}, negates the search and seeks for "NOT SEARCH
#'   CRITERIA". Default is \code{FALSE}.
#' @note To be able to use this functionality, the server must support the
#'   \code{WITHIN} capability.
#' @family custom search
#' @examples
#' \dontrun{
#' # select folder & search
#' con$select_folder(name = "INBOX")
#' # search for messages containing the string "XYZ@@k-state.edu" in the
#' #   "FROM" field AND those that are OLDER than 3600 seconds (1 hour).
#' res <- con$search(request = AND(string(expr = "XYZ@@k-state.edu",
#'                                       where = "FROM"),
#'                                older_than(seconds = 3600)))
#' }
#'
#' @export
#'
older_than <- function(seconds, negate = FALSE) {

  check_args(seconds, negate)

  # setting part of the search string

  if (!isTRUE(negate)) {
    out = paste0('(OLDER ', seconds, ')')

  } else {
    out = paste0('(NOT (OLDER ', seconds, '))')

  }

  return(out)

}
