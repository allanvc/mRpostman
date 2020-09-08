#' @inherit before
#' @family custom search
#' @examples
#' \dontrun{
#' # select folder & search
#' con$select_folder(name = "INBOX")
#' # search for messages SINCE "17-Apr-2019" AND SMALLER than 512KB.
#' res <- con$search(request = OR(on(date_char = "30-Jun-2019"),
#'                                on(date_char = "22-Mar-2018")))
#' # search for messages received ON "30-Jun-2019" OR ON "22-Mar-2018".
#'
#' }
#' @export
#'
on <- function(date_char, negate = FALSE) {


  check_args(date_char, negate)

  # setting part of the search string

  if (!isTRUE(negate)) {
    out = paste0('(ON ', date_char, ')')

  } else {
    out = paste0('(NOT (ON ', date_char, '))')

  }

  return(out)

}
