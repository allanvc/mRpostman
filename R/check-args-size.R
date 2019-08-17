#' @inherit check_args_date title description return
#'
#' @param size An integer specifying the size (in number of characters) of
#'     message to be used as search criterion.
#' @param negate If \code{TRUE}, negates the search and seeks for
#'     "NOT search_criterion". Default is \code{FALSE}.
#'
#' @family customsearch checkargs functions
#'
#' @keywords internal
#'
check_args_size <- function(size, negate) {

  assertthat::assert_that(
    is.numeric(size),
    msg='"size" must be of type numeric.')
  # can it be double? e.g. 3600.5

  assertthat::assert_that(
    is.logical(negate),
    msg='"negate" must be a logical.')

  return(NULL)

}
