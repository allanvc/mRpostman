#' @inherit before
#'
#' @inheritParams check_args_size
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
#'                     smaller_than(
#'                        size = 512000)
#'                    )
#'                  )
#' # searches for messages containing the string "allanvcq@@gmail.com" in the
#' # field "FROM" OR those Smaller Than 512KB.
#'
#' }
#'
#' @export
#'
smaller_than <- function(size, negate = FALSE) {

  check_args_size(size, negate)

  # setting part of the search string

  if (!isTRUE(negate)) {
    out = paste0('(SMALLER ', size, ')')

  } else {
    out = paste0('(NOT (SMALLER ', size, '))')

  }

  return(out)

}
