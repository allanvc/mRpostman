#' @title Fetch Message Metadata
#'
#' @description Fetch Metadata of one or more messages.
#'
#' @inheritParams check_args_fetch_msg_metadata
#'
#' @return A \code{list} or/and text files containing the fetch results.
#'
#' @family fetch
#'
#' @examples
#'
#' \dontrun{
#'
#' # configure IMAP
#' library(mRpostman)
#' configure_imap(url="imaps://imap.gmail.com",
#'                username="your_gmail_user",
#'                password=rstudioapi::askForPassword()
#'               )
#'
#' select_folder(name = "INBOX")
#'
#' # fetching
#' results <- search_sent_since(date_char = "26-Aug-2008") %>%
#'     fetch_msg_metadata(by = "UID",
#'                        metadata = c("INTERNALDATE", "UID", "ENVELOPE",
#'                                     "FLAGS", "RFC822.SIZE", "BODYSTRUCTURE"))
#'
#' }
#'
#' @export
#'
fetch_msg_metadata <- function(msg_id, by = "MSN", metadata,
                               write_to_disk = FALSE, keep_in_mem = TRUE,
                               retries = 2) {

  #check
  check_args_fetch_msg_metadata(msg_id, by, metadata,
                                write_to_disk, keep_in_mem, retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  ## forcing server url to the format we need -- removing unnecessary final slashe(s)
  #.. and doing URL encoding
  url <- utils::URLencode(gsub("/+$", "", IMAP_conn$imapconf$url))

  # isolating the handle
  h <- IMAP_conn$imapconf$conn_handle

  msg_list <- loop_fetch_msg_metadata(url, msg_id, by, metadata,
                                      write_to_disk, keep_in_mem, retries, handle = h)

  # msg_list <- clean_messages(msg_list)

  # handle sanitizing
  rm(h)
  return(msg_list)

}
