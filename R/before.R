#' @title Criteria Helper Functions for Custom Search
#'
#' @description Criteria helper functions to be combined inside a operator helper
#'     function as a custom request in \link{custom_search}.
#'
#' @inheritParams  check_args_date
#'
#' @return A search string to be used as a \code{custom_request} parameter in
#'     \link{custom_search} function.
#'
#' @family customsearch helper functions
#'
#' @examples
#'
#' \dontrun{
#'
#' # configure IMAP
#' library(mRpostman)
#' imapconf <- configure_imap(url="imaps://imap.gmail.com",
#'                            username="your_gmail_user",
#'                            password=rstudioapi::askForPassword()
#'                           )
#'
#' # search
#' result <- imapconf %>%
#'     select_mailbox(mbox = "INBOX") %>%
#'     custom_search(custom_request = AND(before(date_char = "17-Apr-2019"),
#'                                        smaller_than(size = 512000),
#'                                        negate = TRUE))
#' # searches for messages NOT Before "17-Apr-2019" AND NOT Smaller Than  512KB.
#'
#' }
#'
#' @export
#'
before <- function(date_char, negate = FALSE) {


  check_args_date(date_char, negate)

  # setting part of the search string

  if (!isTRUE(negate)) {
    out = paste0('(BEFORE ', date_char, ')')

  } else {
    out = paste0('(NOT (BEFORE ', date_char, '))')

  }

  return(out)

}
