#' @inherit check_args_date title description return
#'
#' @param seconds An integer specifying the number of seconds to be used as
#'     search criterion.
#' @param negate If \code{TRUE}, negates the search and seeks for
#'     "NOT search_criterion". Default is \code{FALSE}.
#'
#' @family customsearch checkargs functions
#'
#' @keywords internal
#'
check_args_within <- function(seconds, negate) {

  assertthat::assert_that(
    is.numeric(seconds),
    msg='"seconds" must be of type numeric.')
  # can it be double? e.g. 3600.5

  assertthat::assert_that(
    is.logical(negate),
    msg='"negate" must be a logical.')

  return(NULL)

}
