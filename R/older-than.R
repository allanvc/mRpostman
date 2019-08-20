#' @inherit younger_than
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
#'                  OR(string(
#'                        section_or_field = "from", string = "allanvcq@@gmail.com"),
#'                     older_than(
#'                        seconds = 3600)
#'                    )
#'                  )
#' # searches for messages containing the string "allanvcq@@gmail.com" in the
#' # field "FROM" OR messgaes that are "older" than one hour (messages arrived
#' # more than 3600 seconds ago).
#'
#' }
#'
#' @export
#'
older_than <- function(seconds, negate = FALSE) {

  check_args_within(seconds, negate)

  # setting part of the search string

  if (!isTRUE(negate)) {
    out = paste0('(OLDER ', seconds, ')')

  } else {
    out = paste0('(NOT (OLDER ', seconds, '))')

  }

  return(out)

}
