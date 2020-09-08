#' @inherit search_smaller_than
#'
#' @family Size-search operations
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
#' results <- search_larger_than(size = 512000, flag = "UNSEEN") # larger than 512KB
#'
#' }
#'
#' @export
#'
search_larger_than <- function(size, negate = FALSE, by = "MSN",
                               flag = NULL, esearch = FALSE, retries = 2) {

  check_args_search_size(size, negate, by, flag, esearch, retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  # forcing server url to the format we need -- removing unnecessary final slashe(s)
  #.. and doing URL encoding
  url <- utils::URLencode(gsub("/+$", "", IMAP_conn$imapconf$url))

  # isolating the handle
  h <- IMAP_conn$imapconf$conn_handle

  #define customrequest
  define_out <- define_searchrequest_size(operation = "LARGER",
                                 size = size, negate = negate,
                                 by = by, flag = flag, esearch = esearch, handle = h)

  h <- define_out$handle
  customrequest <- define_out$customrequest

  response <- execute_search(url = url, handle = h, customrequest = customrequest,
                             esearch, retries)

  return(response)

}
