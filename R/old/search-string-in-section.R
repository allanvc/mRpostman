#' @title Search By String
#'
#' @description Searches for messages containing a string in an specific
#'     section or field.
#'
#' @inheritParams check_args_search_string_in_section
#'
#' @inherit search_before return
#'
#' @family String-search operation
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
#' results <- search_string_in_section(string = "Kansas State University",
#'                                     field = "BODY")
#'
#'
#' }
#'
#' @export
#'
search_string_in_section <- function(string, section, negate = FALSE, by = "MSN",
                          esearch = FALSE, retries = 2) {

  check_args_search_string_in_section(string, section, negate, by, esearch, retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  # forcing server url to the format we need -- removing unnecessary final slashe(s)
  #.. and doing URL encoding
  url <- utils::URLencode(gsub("/+$", "", IMAP_conn$imapconf$url))

  # isolating the handle
  h <- IMAP_conn$imapconf$conn_handle

  #define customrequest
  define_out <- define_searchrequest_string_in_section(string = string, section = section,
                                              negate = negate, by = by,
                                              esearch = esearch, handle = h)

  h <- define_out$handle
  customrequest <- define_out$customrequest

  response <- execute_search(url = url, handle = h, customrequest = customrequest,
                             esearch, retries)

  return(response)

}
