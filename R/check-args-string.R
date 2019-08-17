#' @inherit check_args_date title description return
#'
#' @param string A character string specifying the word or expression to
#'     search for in messages.
#' @param section_or_field A mandatory character string specifying in which
#'     messages's Section or Header Field to search for the provided string.
#'     For some available options, see \link{section_or_field_options}.
#' @param negate If \code{TRUE}, negates the search and seeks for
#'     "NOT search_criterion". Default is \code{FALSE}.
#'
#' @family customsearch checkargs functions
#'
#' @keywords internal
#'
check_args_string <- function(string, section_or_field, negate) {

  assertthat::assert_that(
    is.character(string),
    msg='"string" argument must be a character.')

  assertthat::assert_that(
    is.character(section_or_field),
    msg='"section_or_field" argument must be a character.See section_field_options().')

  assertthat::assert_that(
    is.logical(negate),
    msg='"negate" must be a logical.')

    return(NULL)

}
