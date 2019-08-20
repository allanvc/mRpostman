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
#' imapconf <- configure_imap(url="imaps://imap.gmail.com",
#'                            username="your_gmail_user",
#'                            password=rstudioapi::askForPassword()
#'                           )
#'
#' # fetching and saving results to disk
#' results <- imapconf %>%
#'     select_mailbox(mbox = "INBOX") %>%
#'     search_sent_since(date_char = "26-Aug-2008") %$% #exposition pipe - two argg
#'     fetch_msg_text(imapconf = imapconf, msg_id = msg_id, write_to_disk = TRUE)
#'
#' }
#'
#' @export
#'
fetch_msg_text <- function(imapconf, msg_id, by = "MSN", peek = TRUE,
                         partial = NULL, write_to_disk = FALSE, keep_in_mem = TRUE,
                         try_b64decode = FALSE,
                         retries = 2) {

  #check
  check_args_fetch_msg_text(imapconf, msg_id, by, peek, partial,
                            write_to_disk, keep_in_mem, try_b64decode,
                            retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  # copying imapconf to return the original in the end
  new_imapconf = imapconf

  # config handle
  h <- config_handle(new_imapconf)

  msg_list <- loop_fetch_msg_text(new_imapconf, msg_id, by, peek, partial,
                                  write_to_disk, keep_in_mem,
                                  try_b64decode, retries, handle = h)

  # msg_list <- clean_messages(msg_list)

  # handle sanitizing
  rm(h)
  return(msg_list)
}
