#' Criterion constructor function to be combined in a custom search statement
#' @param expr A character string specifying the word or expression to search
#'   for in messages.
#' @param where A mandatory character string specifying in which
#'   message's Section or Header Field to search for the provided string.
#' @param negate If \code{TRUE}, negates the search and seeks for "NOT SEARCH
#'   CRITERIA". Default is \code{FALSE}.
#' @family custom search
#' @examples
#' \dontrun{
#' # select folder & search
#' con$select_folder(name = "INBOX")
#' # search for messages containing the string "XYZ@@k-state.edu" in the
#' #   "FROM" AND the string "@@gmail.com" in the "CC" field.
#' res <- con$search(request = AND(string(expr = "XYZ@@k-state.edu",
#'                                       where = "FROM"),
#'                                string(expr = "@@gmail.com",
#'                                       where = "CC")))
#' }
#'
#' @export
#'
string <- function(expr, where, negate = FALSE) {

  # Note to self:  all helper functions (even internal) should NOT be declared as
  #.. methods in the R6 class !!!

  # section_or_field = toupper(section_or_field)

  check_args(expr = expr, where = where, negate = negate)

  # setting part of the search string
  section_or_field = where # one is going to be NULL


  if (!isTRUE(negate)) {
    out = paste0('(', section_or_field, ' ', paste0('"', expr, '"'), ')')

  } else {
    out = paste0('(NOT (', section_or_field, ' ', paste0('"', expr, '"'), '))')

  }

  return(out)

}
