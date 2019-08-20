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
#'     custom_search(custom_request = AND(since(date_char = "17-Apr-2019"),
#'                                        smaller_than(size = 512000)
#'                                       ))
#' # searches for messages Since "17-Apr-2019" AND Smaller Than  512KB.
#'
#' }
#'
#' @export
#'
since <- function(date_char, negate = FALSE) {


  check_args_date(date_char, negate)

  # setting part of the search string

  if (!isTRUE(negate)) {
    out = paste0('(SINCE ', date_char, ')')

  } else {
    out = paste0('(NOT (SINCE ', date_char, '))')

  }

  return(out)

}
