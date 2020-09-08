#' @inherit before
#'
#' @inheritParams check_args_string_in_section
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
#'                  OR(string_in_section(string = "Kansas State University"
#'                        section = "text"),
#'                     string_in_header(string = "U.C. Riverside",
#'                        section = "text")
#'                    )
#'                  )
#'
#' }
#'
#' @export
#'
string_in_section <- function(string, section, negate = FALSE) {

  section = toupper(section)

  check_args_string_in_section(section, string, negate)

  # setting part of the search string

  if (!isTRUE(negate)) {
    out = paste0('(', section, ' ', paste0('"', string, '"'), ')')

  } else {
    out = paste0('(NOT (', section, ' ', paste0('"', string, '"'), '))')

  }

  return(out)

}
