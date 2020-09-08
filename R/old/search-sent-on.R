#' @inherit search_sent_before
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
#' # search 1
#' result1 <- search_sent_on(date_char = "17-Dec-2003")
#'
#' # search 2 (mailboxes with compound names)
#' result2 <- search_sent_on(date_char = "17-Sep-2018", flag = "UNDELETED")
#'
#' }
#'
#' @export
#'
search_sent_on <- function(date_char, negate = FALSE, by = "MSN", flag = NULL,
                           esearch = FALSE, retries = 2) {

  check_args_search_date(date_char, negate, by, flag, esearch, retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  # forcing server url to the format we need -- removing unnecessary final slashe(s)
  #.. and doing URL encoding
  url <- utils::URLencode(gsub("/+$", "", IMAP_conn$imapconf$url))

  # isolating the handle
  h <- IMAP_conn$imapconf$conn_handle

  #define customrequest
  define_out <- define_searchrequest_date(operation = "SENTON",
                                          date_char = date_char, negate = negate,
                                          by = by, flag = flag, esearch = esearch,
                                          handle = h)

  h <- define_out$handle
  customrequest <- define_out$customrequest

  response <- execute_search(url = url, handle = h, customrequest = customrequest,
                             esearch, retries)

  return(response)

}
