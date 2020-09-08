#' @inherit AND
#' @family custom search
#' @examples
#' \dontrun{
#' # select folder & search
#' con$select_folder(name = "INBOX")
#' # search for messages SINCE "30-Ago-2019" OR SMALLER than 512KB.
#' res <- con$search(request = OR(sent_since(date_char = "30-Ago-2019"),
#'                                 smaller_than(size = 512000)))
#' }
#' @export
#'
OR <- function(..., negate = FALSE) {

  # ... must be 2+ args
  argg <- list(...)

  assertthat::assert_that(
    length(argg) >= 2
    , msg='OR() requires two or more arguments.')


  assertthat::assert_that(
    is.logical(negate),
    msg='"negate" must be a logical.')

  # setting number of OR's
  n_OR <- length(argg) - 1

  # setting part of the search string

  if (!isTRUE(negate)) {
    out = paste0(paste0(rep('OR', n_OR), collapse = ' '), ' ', paste(..., sep = ' ')) # we'll already have parenthesis
    #... inside '...' args

  } else {
    out = paste0('NOT (', paste0(rep('OR', n_OR), collapse = ' '), ' ', paste(..., sep = ' '), ')')

  }

  return(out)

}
