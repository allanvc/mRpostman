#' @inherit before
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
#'     custom_search(custom_request = OR(sentOn(date_char = "17-Apr-2019"),
#'                                       sentOn(date_char = "17-Jul-2018")
#'                                      ))
#' # searches for messages Sent On "17-Apr-2019" OR Sent On "17-Jul-2018".
#'
#' }
#'
#' @export
#'
sent_on <- function(date_char, negate = FALSE) {


  check_args_date(date_char, negate)

  # setting part of the search string

  if (!isTRUE(negate)) {
    out = paste0('(SENTON ', date_char, ')')

  } else {
    out = paste0('(NOT (SENTON ', date_char, '))')

  }

  return(out)

}
