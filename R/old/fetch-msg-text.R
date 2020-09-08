#' @title Fetch Message Text
#'
#' @description Fetch Text section of one or more messages.
#'
#' @inheritParams check_args_fetch_msg_text
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
#' # fetching and saving results to disk
#' results <- search_sent_since(date_char = "26-Aug-2008") %>%
#'     fetch_msg_text(write_to_disk = TRUE)
#'
#' }
#'
#' @export
#'
fetch_msg_text <- function(msg_id, by = "MSN", peek = TRUE, partial = NULL,
                           write_to_disk = FALSE, keep_in_mem = TRUE,
                           try_b64decode = FALSE, retries = 2) {

  #check
  check_args_fetch_msg_text(msg_id, by, peek, partial, write_to_disk,
                            keep_in_mem, try_b64decode, retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  ## forcing server url to the format we need -- removing unnecessary final slashe(s)
  #.. and doing URL encoding
  url <- utils::URLencode(gsub("/+$", "", IMAP_conn$imapconf$url))

  # isolating the handle
  h <- IMAP_conn$imapconf$conn_handle

  msg_list <- loop_fetch_msg_text(url, msg_id, by, peek, partial,
                                  write_to_disk, keep_in_mem,
                                  try_b64decode, retries, handle = h)

  # msg_list <- clean_messages(msg_list)

  # handle sanitizing
  rm(h)
  return(msg_list)
}
