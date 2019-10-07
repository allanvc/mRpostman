#' @title Relational Operators - Helper Functions for Custom Search
#'
#' @description Allows a combination of criteria helper
#'     functions such as such as \link{before}, \link{since},
#'     \link{on}, \link{sent_before}, \link{sent_since}, \link{sent_on},
#'     \link{flag}, \link{string}, \link{smaller_than}, \link{larger_than},
#'     \link{younger_than}, or \link{younger_than}, in order to execute a
#'     multiple criteria custom search.
#'
#' @param ... a combination of criteria helper functions with its arguments.
#' @param negate If \code{TRUE}, negates the search and seeks for
#'     "NOT search_criterion". Default is \code{FALSE}.
#'
#' @return A search string to be used as a \code{custom_request} parameter in
#'     \link{custom_search} function.
#'
#' @family customsearch helper functions
#'
#' @examples
#'
#' \dontrun{
#'
#' # configure IMAP
#' library(mRpostman)
#' imapconf <- configure_imap(url="imaps://your.imap.server.com",
#'                            username="your_username",
#'                            password=rstudioapi::askForPassword()
#'                           )
#'
#' # search
#' result <- imapconf %>%
#'     select_mailbox(mbox = "INBOX") %>%
#'     custom_search(custom_request = AND(sent_since(date.char = "17-Apr-2019"),
#'                                        smaller_than(size = 512000),
#'                                        negate = TRUE))
#' # searches for messages NOT SentSince "17-Apr-2019" AND NOT Smaller Than 512KB.
#'
#' }
#'
#' @export
#'
AND <- function(..., negate = FALSE) {
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

  if (!isTRUE(negate)) {
    out = paste0(paste(..., sep=' ')) # we'll already have parenthesis
    #... inside '...' args

  } else {
    out = paste0('NOT (', paste(..., sep = ' '), ')')

  }

  return(out)

}
