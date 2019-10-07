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
#' imapconf <- configure_imap(url="imaps://your.imap.server.com",
#'                            username="your_username",
#'                            password=rstudioapi::askForPassword()
#'                           )
#'
#' # search
#' result <- imapconf %>%
#'     select_mailbox(mbox = "INBOX") %>%
#'     custom_search(custom_request = OR(sent_since(date_char = "17-Apr-2019"),
#'                                       smaller_than(size = 512000)
#'                                      ))
#' # searches for messages sentSince "17-Apr-2019" OR Smaller Than 512KB.
#'
#' }
#'
#' @export
#'
sent_since <- function(date_char, negate = FALSE) {


  check_args_date(date_char, negate)

  # setting part of the search string

  if (!isTRUE(negate)) {
    out = paste0('(SENTSINCE ', date_char, ')')

  } else {
    out = paste0('(NOT (SENTSINCE ', date_char, '))')

  }

  return(out)

}
