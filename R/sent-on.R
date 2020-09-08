#' @inherit before
#' @family custom search
#' @examples
#' \dontrun{
#' # select folder & search
#' con$select_folder(name = "INBOX")
#' # search for messages SINCE "30-Ago-2019" OR LARGER than 512KB.
#' res <- con$search(request = OR(sent_since(date_char = "30-Jun-2020"),
#'                                larger_than(size = 512000)))
#' }
#' @export
#'
sent_on <- function(date_char, negate = FALSE) {


  check_args(date_char, negate)

  # setting part of the search string

  if (!isTRUE(negate)) {
    out = paste0('(SENTON ', date_char, ')')

  } else {
    out = paste0('(NOT (SENTON ', date_char, '))')

  }

  return(out)

}
