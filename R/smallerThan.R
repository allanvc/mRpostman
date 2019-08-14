#' @title Criteria Helper Functions for Custom Search
#'
#' @description Criteria helper functions to be combined inside a operator helper
#'     function as a custom request in \link{customSearch}.
#'
#' @inheritParams check_args_size
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
#'                  OR(String(
#'                        section_or_field = "from", string = "allanvcq@@gmail.com"),
#'                     smallerThan(
#'                        size = 512000)
#'                    )
#'                  )
#' # searches for messages containing the string "allanvcq@@gmail.com" in the field "FROM"
#'     OR those smallerThan 512KB.
#'
#' }
#'
#' @export
#'
smallerThan <- function(size, negate = FALSE){

  check_args_size(size, negate)

  # setting part of the search string

  if(!isTRUE(negate)){
    out = paste0('(SMALLER ', size, ')')

  } else{
    out = paste0('(NOT (SMALLER ', size, '))')

  }

  return(out)

}
