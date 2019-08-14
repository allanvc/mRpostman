#' @title Fetch Message Text
#'
#' @description Fetch Text section of one or more messages.
#'
#' @inheritParams check_args_fetchMsgText
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
#' # fetching and saving results to disk
#' results <- imapconf %>%
#'     selectMailbox(mbox = "INBOX") %>%
#'     searchSentSince(date_char = "26-Aug-2008") %$% #exposition pipe - two argg
#'     fetchMsgText(imapconf = imapconf, msg_id = msg_id, write_to_file = TRUE)
#'
#' }
#'
#' @export
#'
fetchMsgText <- function(imapconf, msg_id, by = "MSN", peek = TRUE,
                         partial = NULL, write_to_file = FALSE, keep_in_mem = TRUE,
                         retries = 2){

  #check
  check_args_fetchMsgText(imapconf, msg_id, by, peek, partial,
                               write_to_file, keep_in_mem, retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  # copying imapconf to return the original in the end
  new_imapconf = imapconf

  # config handle
  h <- config_handle(new_imapconf)

  msg_list <- loop_fetchMsgText(new_imapconf, msg_id, by, peek, partial,
                                     write_to_file, keep_in_mem, retries, handle = h)

  # msg_list <- clean_messages(msg_list)

  # handle sanitizing
  rm(h)
  return(msg_list)
}
