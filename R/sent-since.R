#' @inherit before
#' @family custom search
#' @examples
#' \dontrun{
#' # select folder & search
#' con$select_folder(name = "INBOX")
#' # search for messages SENT SINCE "22-Mar-2020" OR containing the STRING
#' #  "congratulations" in the subject.
#' res <- con$search(request = AND(sent_since(date_char = "22-Mar-2020"),
#'                                 string(expr = "congratulations",
#'                                        where = "SUBJECT")))
#' }
#' @export
#'
sent_since <- function(date_char, negate = FALSE) {


  check_args(date_char, negate)

  # setting part of the search string

  if (!isTRUE(negate)) {
    out = paste0('(SENTSINCE ', date_char, ')')

  } else {
    out = paste0('(NOT (SENTSINCE ', date_char, '))')

  }

  return(out)

}
