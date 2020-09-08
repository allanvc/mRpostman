#' @inherit before
#'
#' @inheritParams check_args_string
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
#'                  OR(string(
#'                        section_or_field = "from", string = "allanvcq@@gmail.com"),
#'                     string(
#'                        section_or_field = "to", string = "quadros@@ksu.edu")
#'                    )
#'                  )
#' # searches for messages containing the string "allanvcq@@gmail.com" OR
#' # "allanvcq@@yahoo.com" in the "FROM" field.
#'
#' }
#'
#' @export
#'
string <- function(expr, in_headerfield = NULL, in_section = NULL, negate = FALSE) {

  # Note to self:  all helper functions (even internal) should not be declared as
  #.. methods in the R6 class !!!

  # section_or_field = toupper(section_or_field)

  check_argg(expr = expr, in_headerfield = in_headerfield, in_section = in_section,
             negate)

  # setting part of the search string
  section_or_field = paste0(in_headerfield, in_section) # one is going to be NULL


  if (!isTRUE(negate)) {
    out = paste0('(', section_or_field, ' ', paste0('"', expr, '"'), ')')

  } else {
    out = paste0('(NOT (', section_or_field, ' ', paste0('"', expr, '"'), '))')

  }

  return(out)

}
