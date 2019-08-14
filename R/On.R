#' @inherit Before
#'
#' @family search
#' @family custom
#' @family criteria helper functions
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
#'     customSearch(custom_request = OR(On(date_char = "17-Apr-2019"),
#'                                      On(date_char = "17-Jul-2018")))
#' # searches for messages On "17-Apr-2019" OR On "17-Jul-2018".
#'
#' }
#'
#' @export
#'
On <- function(date_char, negate = FALSE){


  check_args_date(date_char, negate)

  # setting part of the search string

  if(!isTRUE(negate)){
    out = paste0('(ON ', date_char, ')')

  } else{
    out = paste0('(NOT (ON ', date_char, '))')

  }

  return(out)

}
