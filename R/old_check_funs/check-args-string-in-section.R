#' @inherit check_args_date title description return
#'
#' @param string A character string specifying the word or expression to
#'     search for in messages.
#' @param section A mandatory character string specifying in which Header Field
#'     of the message to search for the provided string.
#' @param negate If \code{TRUE}, negates the search and seeks for
#'     "NOT search_criterion". Default is \code{FALSE}.
#'
#' @family customsearch checkargs functions
#'
#' @keywords internal
#'
check_args_string_in_section <- function(string, section, negate) {

  assertthat::assert_that(
    is.character(string),
    msg='"string" argument must be a character.')

  assertthat::assert_that(
    is.character(section),
    msg='"section" argument must be a character.')

  assertthat::assert_that(
    is.logical(negate),
    msg='"negate" must be a logical.')

    return(NULL)

}
