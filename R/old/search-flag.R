#' @title Search By Flag
#'
#' @description Searches for messages marked with a specific flag.
#'
#' @inheritParams check_args_search_flag
#'
#' @inherit search_before return
#'
#' @family Flag-search operations
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
#' results <- search_flag(flag = "RECENT", by = "UID")
#'
#' }
#'
#' @export
#'
search_flag <- function(flag, negate = FALSE, by = "MSN", esearch = FALSE,
                        retries = 2) {

  check_args_search_flag(flag, negate, by, esearch, retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  # forcing server url to the format we need -- removing unnecessary final slashe(s)
  #.. and doing URL encoding
  url <- utils::URLencode(gsub("/+$", "", IMAP_conn$imapconf$url))

  # isolating the handle
  h <- IMAP_conn$imapconf$conn_handle

  #define customrequest
  define_out <- define_searchrequest_flag(flag = flag, negate = negate,
                                 by = by, esearch = esearch, handle = h)

  h <- define_out$handle
  customrequest <- define_out$customrequest

  response <- execute_search(url = url, handle = h, customrequest = customrequest,
                             esearch, retries)

  return(response)

}
