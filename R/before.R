#' Criterion constructor function to be combined in a custom search statement
#' @param date_char A \code{character string} with format "DD-Mon-YYYY", e.g.
#'   "01-Apr-2019". We opt not to use \code{Date} or \code{POSIX*} like
#'   objects, since IMAP servers use this unusual date format.
#' @param negate If \code{TRUE}, negates the search and seeks for "NOT SEARCH
#'   CRITERIA". Default is \code{FALSE}.
#' @return A search string to be used as a \code{request} parameter in
#'  \code{ImapCon$search()} function.
#' @family custom search
#' @examples
#' \dontrun{
#' # select folder & search
#' con$select_folder(name = "INBOX")
#' # search for messages BEFORE "17-Apr-2019" AND NOT SMALLER than 512KB.
#' res <- con$search(request = AND(before(date_char = "17-Apr-2019"),
#'                                 smaller_than(size = 512000, negate = TRUE)))
#' }
#' @export
#'
before <- function(date_char, negate = FALSE) {


  check_args(date_char, negate)

  # setting part of the search string

  if (!isTRUE(negate)) {
    out = paste0('(BEFORE ', date_char, ')')

  } else {
    out = paste0('(NOT (BEFORE ', date_char, '))')

  }

  return(out)

}
