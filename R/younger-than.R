#' @inherit before
#'
#' @inheritParams check_args_within
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
#'     custom_search(custom_request =
#'                  OR(smaller_than(
#'                        size = 512000),
#'                     younger_than(
#'                        seconds = 3600)
#'                    )
#'                  )
#' # searches for messages that are Smaller Than 512 KB OR Younger Than 3600
#' # seconds (or one hour).
#'
#' }
#'
#' @export
#'
younger_than <- function(seconds, negate = FALSE) {

  check_args_within(seconds, negate)

  # setting part of the search string

  if (!isTRUE(negate)) {
    out = paste0('(YOUNGER ', seconds, ')')

  } else {
    out = paste0('(NOT (YOUNGER ', seconds, '))')

  }

  return(out)

}
