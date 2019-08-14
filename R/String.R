#' @title Criteria Helper Functions for Custom Search
#'
#' @description Criteria helper functions to be combined inside a operator helper
#'     function as a custom request in \link{customSearch}.
#'
#' @inheritParams check_args_string
#'
#' @return A search string to be used as a \code{custom_request} parameter in
#'     \link{customSearch} function.
#'
#' @family customsearch helper functions
#'
#' @examples
#'
#' \dontrun{
#'
#' # configure IMAP
#' library(mRpostman)
#' imapconf <- configureIMAP(url="imaps://imap.gmail.com",
#'                           username="your_gmail_user",
#'                           password=rstudioapi::askForPassword()
#'                           )
#'
#' # search
#' result <- imapconf %>%
#'     selectMailbox(mbox = "INBOX") %>%
#'     customSearch(custom_request =
#'                  OR(String(
#'                        section_or_field = "from", string = "allanvcq@@gmail.com"),
#'                     String(
#'                        section_or_field = "from", string = "allanvcq@@yahoo.com")
#'                    )
#'                  )
#' # searches for messages containing the string "allanvcq@@gmail.com" OR
#'     "allanvcq@@yahoo.com" in the "FROM" field.
#'
#' }
#'
#' @export
#'
String <- function(section_or_field, string, negate = FALSE){

  section_or_field = toupper(section_or_field)

  check_args_string(section_or_field, string, negate)

  # setting part of the search string

  if(!isTRUE(negate)){
    out = paste0('(', section_or_field, ' ', paste0('"', string, '"'), ')')

  } else{
    out = paste0('(NOT (', section_or_field, ' ', paste0('"', string, '"'), '))')

  }

  return(out)

}
