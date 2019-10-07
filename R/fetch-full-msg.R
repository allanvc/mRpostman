#' @title Fetch Full Messages
#'
#' @description Fetch all the sections and fields of one or more messages.
#'
#' @inheritParams check_args_fetch_full_msg
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
#' imapconf <- configure_imap(url="imaps://your.imap.server.com",
#'                            username="your_username",
#'                            password=rstudioapi::askForPassword()
#'                           )
#'
#' # fetching
#' results <- imapconf %>%
#'     select_mailbox(mbox = "TAM") %>%
#'     search_before(date_char = "10-may-2012", by = "UID") %$% #exposition pipe - two argg
#'     fetch_full_msg(imapconf = imapconf,
#'                    msg_id = msg_id,
#'                    by="UID", write_to_disk = TRUE,
#'                    keep_in_mem = FALSE,
#'                    partial = "0.789")
#'
#' }
#'
#' @export
#'
fetch_full_msg <- function(imapconf, msg_id, by = "MSN", peek = TRUE,
                           partial = NULL, write_to_disk = FALSE, keep_in_mem = TRUE,
                           retries = 2) {

  #check
  check_args_fetch_full_msg(imapconf, msg_id, by, peek, partial,
                            write_to_disk, keep_in_mem, retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  # copying imapconf to return the original in the end
  new_imapconf = imapconf

  # config handle
  h <- config_handle(new_imapconf)

  if (isFALSE(keep_in_mem)) {
    warning('Setting "keep_in_mem = TRUE" will not alow you to use get_attachment')
  }

  msg_list <- loop_fetch_full_msg(new_imapconf, msg_id, by, peek, partial,
                                  write_to_disk, keep_in_mem, retries, handle = h)

  # msg_list <- clean_messages(msg_list)

  # v0.3.0.0 -- added a attribute mbox to fetch_full_msg so we can use get_attachment()
  #after it
  attr(msg_list, which = 'mbox') = new_imapconf$mbox

  # handle sanitizing
  rm(h)
  return(msg_list)
}
