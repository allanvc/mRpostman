#' @title Custom Search Specific Check
#'
#' @description Internal helper function for checking specific arguments
#'     used in \link{custom_search}.
#'
#' @param date_char A character vector with format "DD-Mon-YYYY",
#'     e.g. "01-Apr-2019". We opted not to use objects of type "date", since IMAP
#'     servers like this not so common date format.
#' @param negate If \code{TRUE}, negates the search and seeks for
#'     "NOT search_criterion". Default is \code{FALSE}.
#'
#' @return \code{NULL} if arguments are correct.
#'
#' @family customsearch checkargs functions
#'
#' @keywords internal
#'
check_args_date <- function(date_char, negate) {

  assertthat::assert_that(
    stringr::str_detect(string = date_char,
                        pattern = "[\\d]{2}-[a-zA-Z]{3}-[\\d]{4}"),
    msg='"date_char" must be of type character with format DD-Mon-YYYY", e.g. "01-Apr-2019".')

  assertthat::assert_that(
    is.logical(negate),
    msg='"negate" must be a logical.')

  return(NULL)

}
