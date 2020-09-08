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
#' configure_imap(url="imaps://imap.gmail.com",
#'                username="your_gmail_user",
#'                password=rstudioapi::askForPassword()
#'               )
#'
#' select_folder(name = "TAM")
#'
#' # fetching
#' results <- search_before(date_char = "10-may-2012", by = "UID") %>%
#'     fetch_full_msg(by="UID", write_to_disk = TRUE,
#'                    keep_in_mem = FALSE,
#'                    partial = "0.789")
#'
#' }
#'
#' @export
#'
fetch_full_msg <- function(msg_id, by = "MSN", peek = TRUE, partial = NULL,
                           write_to_disk = FALSE, keep_in_mem = TRUE, retries = 2) {

  #check
  check_args_fetch_full_msg(msg_id, by, peek, partial, write_to_disk,
                            keep_in_mem, retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  ## forcing server url to the format we need -- removing unnecessary final slashe(s)
  #.. and doing URL encoding
  url <- utils::URLencode(gsub("/+$", "", IMAP_conn$imapconf$url))

  # isolating the handle
  h <- IMAP_conn$imapconf$conn_handle

  if (isFALSE(keep_in_mem)) {
    warning('Setting "keep_in_mem = TRUE" will not alow you to use get_attachments()')
  }

  msg_list <- loop_fetch_full_msg(url, msg_id, by, peek, partial,
                                  write_to_disk, keep_in_mem, retries, handle = h)

  # msg_list <- clean_messages(msg_list)

  # v0.3.0.0 -- added a attribute mbox to fetch_full_msg so we can use get_attachment()
  #after it
  # attr(msg_list, which = 'mbox') = new_imapconf$mbox # mudar na nova versão -- a princípio
  # ... a propria get_attachments buscara do imapconf$folder

  # handle sanitizing
  rm(h)
  return(msg_list)
}
