#' @inherit before
#'
#' @inheritParams check_args_string_in_header
#'
#' @family customsearch helper functions
#'
#' @examples
#'
#' \dontrun{
#'
#' # configure IMAP
#' library(mRpostman)
#' configure_imap(url="imaps://your.imap.server.com",
#'                username="your_username",
#'                password=rstudioapi::askForPassword()
#'               )
#'
#' select_folder(name = "INBOX")
#'
#' # search
#' result <- custom_search(custom_request =
#'                  OR(string_in_header(string = "allanvcq@@gmail.com"
#'                        field = "from"),
#'                     string_in_header(string = "quadros@@ksu.edu",
#'                        field = "to")
#'                    )
#'                  )
#' # searches for messages containing the string "allanvcq@@gmail.com" in the
#' "TO" field OR # "allanvcq@@yahoo.com" in the "FROM" field.
#'
#' }
#'
#' @export
#'
string_in_header <- function(string, field, negate = FALSE) {

  field = toupper(field)

  check_argg(field, string, negate)
  check_argg(field = field, string = string, negate = negate)

  # setting part of the search string

  if (!isTRUE(negate)) {
    out = paste0('(', field, ' ', paste0('"', string, '"'), ')')

  } else {
    out = paste0('(NOT (', field, ' ', paste0('"', string, '"'), '))')

  }

  return(out)

}
