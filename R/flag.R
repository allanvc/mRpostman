#' @title Criteria Helper Functions for Custom Search
#'
#' @inherit before description return
#'
#' @inheritParams check_args_flag
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
#'     custom_search(custom_request = AND(flag("UNSEEN"),
#'                                        smaller_than(size = 512000),
#'                                        negate = TRUE))
#' # searches for messages with Flag "UNSEEN" AND NOT Smaller Than  512KB.
#'
#' }
#'
#' @export
#'
flag <- function(flag, negate = FALSE) {


  check_args_flag(flag, negate)

  # setting part of the search string

  if (!isTRUE(negate)) {
    out = paste0('(', flag, ')')

  } else {
    out = paste0('(NOT (', flag, '))')

  }

  return(out)

}
