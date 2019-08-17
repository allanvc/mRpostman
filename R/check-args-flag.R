#' @inherit check_args_date title description return
#'
#' @param flag A string specifying the flag to be used for filtering messages.
#'     Use \link{flag_options} to list the flags available in your IMAP server.
#' @param negate If \code{TRUE}, negates the search and seeks for
#'     "NOT search_criterion". Default is \code{FALSE}.
#'
#' @family customsearch checkargs functions
#'
#' @keywords internal
#'
check_args_flag <- function(flag, negate) {

  assertthat::assert_that(
    is.character(flag),
    msg='"flag" argument must be a character. See mRpostman::flag_options().')

  assertthat::assert_that(
    is.logical(negate),
    msg='"negate" must be a logical.')

  return(NULL)

}
