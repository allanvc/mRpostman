#' @title Criteria Helper Functions for Custom Search
#'
#' @description Criteria helper functions to be combined inside a operator helper
#'     function as a custom request in \link{customSearch}.
#'
#' @inheritParams check_args_within
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
#'     customSearch(custom_request =
#'                  OR(smallerThan(
#'                        size = 512000),
#'                     youngerThan(
#'                        seconds = 3600)
#'                    )
#'                  )
#' # searches for messages that are youngerThan 3600 seconds (or one hour) OR
#'     that contains the string "allanvcq@@gmail.com" in the field "FROM"
#'
#' }
#'
#' @export
#'
youngerThan <- function(seconds, negate = FALSE){

  check_args_within(seconds, negate)

  # setting part of the search string

  if(!isTRUE(negate)){
    out = paste0('(YOUNGER ', seconds, ')')

  } else{
    out = paste0('(NOT (YOUNGER ', seconds, '))')

  }

  return(out)

}
