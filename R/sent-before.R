#' @inherit before
#' @family custom search
#' @examples
#' \dontrun{
#' # select folder & search
#' con$select_folder(name = "INBOX")
#' # search for messages SINCE "30-Ago-2019" AND SMALLER than 512KB.
#' res <- con$search(request = AND(sent_since(date_char = "30-Ago-2019"),
#'                                 smaller_than(size = 512000)))
#' }
#' @export
#'
sent_before <- function(date_char, negate = FALSE) {


  check_args(date_char, negate)

  # setting part of the search string

  if (!isTRUE(negate)) {
    out = paste0('(SENTBEFORE ', date_char, ')')

  } else {
    out = paste0('(NOT (SENTBEFORE ', date_char, '))')

  }

  return(out)

}
