#' @title Search Within seconds
#'
#' @description Searches for messages within a number of seconds from the
#'     server’s current time to fetch messages.
#'
#' @inheritParams check_args_search_within
#'
#' @inherit search_before return
#'
#' @note Some IMAP servers, such as those from Gmail and AOL, may not support WITHIN SEARCH
#'     EXTENSION.
#'
#' @family Within-search operations
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
#' results <- search_younger_than(seconds = 3600)
#'
#' }
#'
#' @export
#'
search_younger_than <- function(seconds, negate = FALSE, by = "MSN", flag = NULL,
                                esearch = FALSE, retries = 2) {

  check_args_search_within(seconds, negate, by, flag, esearch, retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  # forcing server url to the format we need -- removing unnecessary final slashe(s)
  #.. and doing URL encoding
  url <- utils::URLencode(gsub("/+$", "", IMAP_conn$imapconf$url))

  # isolating the handle
  h <- IMAP_conn$imapconf$conn_handle

  #define customrequest
  define_out <- define_searchrequest_within(operation = "YOUNGER",
                                 seconds = seconds, negate = negate,
                                 by = by, flag = flag, esearch = esearch, handle = h)

  h <- define_out$handle
  customrequest <- define_out$customrequest

  response <- execute_search(url = url, handle = h, customrequest = customrequest,
                             esearch, retries)

  return(response)

}
