#' @title Fetch Message Metadata
#'
#' @description Fetch Metadata of one or more messages.
#'
#' @inheritParams check_args_fetchMsgMetadata
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
#' imapconf <- configureIMAP(url="imaps://imap.gmail.com",
#'                           username="your_gmail_user",
#'                           password=rstudioapi::askForPassword()
#'                           )
#'
#' # fetching
#' results <- imapconf %>%
#'     selectMailbox(mbox = "INBOX") %>%
#'     searchSentSince(date_char = "26-Aug-2008") %$% #exposition pipe - two argg
#'     fetchMsgMetadata(imapconf, msg_id = msg_id, by = "UID",
#'                      metadata = c("INTERNALDATE", "UID", "ENVELOPE", "FLAGS",
#'                                  "RFC822.SIZE", "BODYSTRUCTURE"))
#'
#' }
#'
#' @export
#'
fetchMsgMetadata <- function(imapconf, msg_id, by = "MSN", metadata,
                             write_to_file = FALSE, keep_in_mem = TRUE,
                             retries = 2){

  #check
  check_args_fetchMsgMetadata(imapconf, msg_id, by, metadata,
                              write_to_file, keep_in_mem, retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  # copying imapconf to return the original in the end
  new_imapconf = imapconf

  # config handle
  h <- config_handle(new_imapconf)

  msg_list <- loop_fetchMsgMetadata(new_imapconf, msg_id, by, metadata,
                                    write_to_file, keep_in_mem, retries, handle = h)

  # msg_list <- clean_messages(msg_list)

  # handle sanitizing
  rm(h)
  return(msg_list)

}
