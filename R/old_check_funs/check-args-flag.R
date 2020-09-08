#' @inherit check_args_date title description return
#'
#' @param flag A string specifying the flag to be used for filtering the messages.
#'     Use \code{\link{list_flags}} to list the flags of a specific folder.
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
    msg='"flag" argument must be a character.')

  assertthat::assert_that(
    is.logical(negate),
    msg='"negate" must be a logical.')

  return(NULL)

}
