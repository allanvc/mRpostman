#' @title Search By Internal Date
#'
#' @description Functions that allows searching for messages using the (internal)
#'     date criteria, such as before, since, on, and period.
#'
#' @inheritParams check_args_search_period
#'
#' @inherit search_before return
#'
#' @note \code{\link{search_before}}, \code{\link{search_since}},
#'     \code{\link{search_on}}, and \code{\link{search_period}} use internal date,
#'     which reflects the moment when the message was received.
#'     \code{\link{search_sent_before}}, \code{\link{search_sent_since}},
#'     \code{\link{search_sent_on}}, and \code{\link{search_sent_period}} use
#'     RFC-2822 date header (origination date), which "specifies the date and
#'     time at which the creator of the message indicated that the message was
#'     complete and ready to enter the mail delivery system" (Resnick, 2008).
#'     Dates in both methods must be the same most of time. Nonetheless,
#'     using internal date for search is faster (Babcock, 2016).
#'
#' @references Resnick, P., "Internet Message Format", RFC 5322, October 2008.
#'
#' @references Babcock, N., "Introduction to IMAP", Blog, May 2016.
#'
#' @family Date-search operations
#'
#' @examples
#'
#' \dontrun{
#'
#' # configure IMAP
#' library(mRpostman)
#' configure_imap(url="imaps://your.imap.server.com",
#'                username="your_username",
#'                password=rstudioapi::askForPassword()
#'               )
#'
#' select_folder(name = "INBOX")
#'
#' # search
#' results <- search_period(since_date_char = "17-Apr-2012",
#'                          before_date_char = "30-Jun-2015",
#'                          flag = "DRAFT")
#'
#'
#' }
#'
#' @export
#'
search_period <- function(since_date_char, before_date_char, negate = FALSE,
                          by = "MSN", flag = NULL, esearch = FALSE, retries = 2) {

  check_args_search_period(since_date_char, before_date_char, negate, by, flag,
                           esearch, retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  # forcing server url to the format we need -- removing unnecessary final slashe(s)
  #.. and doing URL encoding
  url <- utils::URLencode(gsub("/+$", "", IMAP_conn$imapconf$url))

  # isolating the handle
  h <- IMAP_conn$imapconf$conn_handle

  #define customrequest
  define_out <- define_searchrequest_period(operation1 = "SINCE",
                                            since_date_char = since_date_char,
                                            operation2 = "BEFORE",
                                            before_date_char = before_date_char,
                                            negate = negate,
                                            by = by, flag = flag,
                                            esearch = esearch, handle = h)

  h <- define_out$handle
  customrequest <- define_out$customrequest

  response <- execute_search(url = url, handle = h, customrequest = customrequest,
                             esearch, retries)

  return(response)

}
