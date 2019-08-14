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
#'     customSearch(custom_request = OR(sentOn(date_char = "17-Apr-2019"),
#'                                      sentOn(date_char = "17-Jul-2018")))
#' # searches for messages sentOn "17-Apr-2019" OR sentOn "17-Jul-2018".
#'
#' }
#'
#' @export
#'
sentOn <- function(date_char, negate = FALSE){


  check_args_date(date_char, negate)

  # setting part of the search string

  if(!isTRUE(negate)){
    out = paste0('(SENTON ', date_char, ')')

  } else{
    out = paste0('(NOT (SENTON ', date_char, '))')

  }

  return(out)

}
