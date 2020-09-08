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
#' @keywords internal
#'
fetch_body_int <- function(self, msg, use_uid, peek, partial, write, silent,
                           retries) {

  print(msg)
  #check
  check_argg(msg = msg, use_uid = use_uid, peek = peek, partial = partial,
             write = write, silent = silent, retries = retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  url <- self$url

  # isolating the handle
  h <- self$con_handle

  msg_list <- loop_fetch_body(self, msg, use_uid, peek, partial, write, retries,
                              handle = h)

  # msg_list <- clean_messages(msg_list)

  # v0.3.0.0 -- added a attribute mbox to fetch_full_msg so we can use get_attachment()
  #after it
  # attr(msg_list, which = 'mbox') = new_imapconf$mbox # mudar na nova versão -- a princípio
  # ... a propria get_attachments buscara do imapconf$folder

  # handle sanitizing
  rm(h)
  if (self$verbose) {
    Sys.sleep(0.01)  # wait for the end of the client-server conversation
  }
  return(msg_list)
}
