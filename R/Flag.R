#' @title Criteria Helper Functions for Custom Search
#'
#' @description Criteria helper functions to be combined inside a operator helper
#'     function as a custom request in \link{customSearch}.
#'
#' @inheritParams check_args_flag
#'
#' @return A search string to be used as a \code{custom_request} parameter in
#'     \link{customSearch} function.
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
#'     customSearch(custom_request = AND(Flag(flag="UNSEEN"),
#'                                       smallerThan(size = 512000),
#'                                       negate = TRUE))
#' # searches for messages with Flag ""UNSEEN" AND NOT SmallerThan  512KB.
#'
#' }
#'
#' @export
#'
Flag <- function(flag, negate = FALSE){


  check_args_flag(flag, negate)

  # setting part of the search string

  if(!isTRUE(negate)){
    out = paste0('(', flag, ')')

  } else{
    out = paste0('(NOT (', flag, '))')

  }

  return(out)

}
