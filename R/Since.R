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
#'     customSearch(custom_request = AND(Since(date_char = "17-Apr-2019"),
#'                                       smallerThan(size = 512000)))
#' # searches for messages Since "17-Apr-2019" AND SmallerThan  512KB.
#'
#' }
#'
#' @export
#'
Since <- function(date_char, negate = FALSE){


  check_args_date(date_char, negate)

  # setting part of the search string

  if(!isTRUE(negate)){
    out = paste0('(SINCE ', date_char, ')')

  } else{
    out = paste0('(NOT (SINCE ', date_char, '))')

  }

  return(out)

}
