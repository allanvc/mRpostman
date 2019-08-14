#' @title Fetch Full Messages
#'
#' @description Fetch all the sections and fields of one or more messages.
#'
#' @inheritParams check_args_fetchFullMsg
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
#'     selectMailbox(mbox = "TAM") %>%
#'     searchBefore(date_char = "10-may-2012", by = "UID") %$% #exposition pipe - two argg
#'     fetchFullMsg(imapconf = imapconf, msg_id = msg_id,
#'                  by="UID", write_to_file = TRUE, keep_in_mem = FALSE,
#'                  partial = "0.789")
#'
#' }
#'
#' @export
#'
fetchFullMsg <- function(imapconf, msg_id, by = "MSN", peek = TRUE,
                              partial = NULL, write_to_file = FALSE, keep_in_mem = TRUE,
                              retries = 2){

  #check
  check_args_fetchFullMsg(imapconf, msg_id, by, peek, partial,
                               write_to_file, keep_in_mem, retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  # copying imapconf to return the original in the end
  new_imapconf = imapconf

  # config handle
  h <- config_handle(new_imapconf)

  msg_list <- loop_fetchFullMsg(new_imapconf, msg_id, by, peek, partial,
                                     write_to_file, keep_in_mem, retries, handle = h)

  # msg_list <- clean_messages(msg_list)

  # handle sanitizing
  rm(h)
  return(msg_list)
}
