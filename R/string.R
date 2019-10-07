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
#' imapconf <- configure_imap(url="imaps://your.imap.server.com",
#'                            username="your_username",
#'                            password=rstudioapi::askForPassword()
#'                           )
#'
#' # search
#' result <- imapconf %>%
#'     select_mailbox(mbox = "INBOX") %>%
#'     custom_search(custom_request =
#'                  OR(string(
#'                        section_or_field = "from", string = "allanvcq@@gmail.com"),
#'                     string(
#'                        section_or_field = "from", string = "allanvcq@@yahoo.com")
#'                    )
#'                  )
#' # searches for messages containing the string "allanvcq@@gmail.com" OR
#' # "allanvcq@@yahoo.com" in the "FROM" field.
#'
#' }
#'
#' @export
#'
string <- function(section_or_field, string, negate = FALSE) {

  section_or_field = toupper(section_or_field)

  check_args_string(section_or_field, string, negate)

  # setting part of the search string

  if (!isTRUE(negate)) {
    out = paste0('(', section_or_field, ' ', paste0('"', string, '"'), ')')

  } else {
    out = paste0('(NOT (', section_or_field, ' ', paste0('"', string, '"'), '))')

  }

  return(out)

}
