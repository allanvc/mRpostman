#' @inherit Before
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
#'     customSearch(custom_request = OR(sentBefore(date_char = "17-Apr-2019"),
#'                                      smallerThan(size = 512000)))
#' # searches for messages sentBefore "17-Apr-2019" OR smallerThan 512KB.
#'
#' }
#'
#' @export
#'
sentBefore <- function(date_char, negate = FALSE){


  check_args_date(date_char, negate)

  # setting part of the search string

  if(!isTRUE(negate)){
    out = paste0('(SENTBEFORE ', date_char, ')')

  } else{
    out = paste0('(NOT (SENTBEFORE ', date_char, '))')

  }

  return(out)

}
