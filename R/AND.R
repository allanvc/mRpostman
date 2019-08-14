#' @title Relational Operators - Helper Functions for Custom Search
#'
#' @description Allows a combination of criteria helper
#'     functions such as \link{Before}, \link{Since}, \link{On},
#'     \link{sentBefore}, \link{sentSince}, \link{sentOn}, \link{Flag}, or
#'     \link{String}, in order to execute a multiple criteria custom search.
#'
#' @param ... a combination of criteria helper functions with its arguments.
#' @param negate If \code{TRUE}, negates the search and seeks for
#'     "NOT search_criterion". Default is \code{FALSE}.
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
#'     customSearch(custom_request = AND(sentSince(date.char = "17-Apr-2019"),
#'                                       smallerThan(size = 512000),
#'                                       negate = TRUE))
#' # searches for messages NOT SentSince "17-Apr-2019" AND NOT SmallerThan  512KB.
#'
#' }
#'
#' @export
#'
AND <- function(..., negate = FALSE){
  # just a wrapper to paste

  # ... must be 2+ args
  argg <- list(...)

  assertthat::assert_that(
    length(argg) >= 2
    , msg='AND() requires two or more arguments.')

  assertthat::assert_that(
    is.logical(negate),
    msg='"negate" must be a logical.')

  # setting part of the search string

  if(!isTRUE(negate)){
    out = paste0(paste(..., sep=' ')) # we'll already have parenthesis
    #... inside '...' args

  } else{
    out = paste0('NOT (', paste(..., sep = ' '), ')')

  }

  return(out)

}
