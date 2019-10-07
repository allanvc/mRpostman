#' @inherit AND
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
#'     custom_search(custom_request = OR(sent_since(date.char="17-Apr-2019"),
#'                                       smaller_than(size = 512000, negate = TRUE)
#'                                      ))
#' # searches for messages SentSince "17-Apr-2019" OR NOT Smaller Than  512KB.
#'
#' }
#'
#' @export
#'
OR <- function(..., negate = FALSE) {

  # ... must be 2+ args
  argg <- list(...)

  assertthat::assert_that(
    length(argg) >= 2
    , msg='OR() requires two or more arguments.')


  assertthat::assert_that(
    is.logical(negate),
    msg='"negate" must be a logical.')

  # setting number of OR's
  n_OR <- length(argg) - 1

  # setting part of the search string

  if (!isTRUE(negate)) {
    out = paste0(paste0(rep('OR', n_OR), collapse = ' '), ' ', paste(..., sep = ' ')) # we'll already have parenthesis
    #... inside '...' args

  } else {
    out = paste0('NOT (', paste0(rep('OR', n_OR), collapse = ' '), ' ', paste(..., sep = ' '), ')')

  }

  return(out)

}
