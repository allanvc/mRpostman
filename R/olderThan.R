#' @inherit youngerThan
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
#'                     olderThan(
#'                        seconds = 3600)
#'                    )
#'                  )
#' # searches for messages containing the string "allanvcq@@gmail.com" in the field "FROM"
#'     OR messgaes that are "older" than one hour (messages arrived 3more than
#'     600 seconds ago).
#'
#' }
#'
#' @export
#'
olderThan <- function(seconds, negate = FALSE){

  check_args_within(seconds, negate)

  # setting part of the search string

  if(!isTRUE(negate)){
    out = paste0('(OLDER ', seconds, ')')

  } else{
    out = paste0('(NOT (OLDER ', seconds, '))')

  }

  return(out)

}
