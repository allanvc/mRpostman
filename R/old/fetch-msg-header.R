#' @title Fetch Message Headers
#'
#' @description Fetch header fields of one or more messages.
#'
#' @inheritParams check_args_fetch_msg_header
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
#' results <- search_before(date_char = "10-may-2012", by = "UID") %>%
#'     fetch_msg_header(fields = c("From", "To"))
#'
#' }
#'
#' @export
#'
fetch_msg_header <- function(msg_id, by = "MSN", fields = NULL,
                             negate_fields = FALSE, peek = TRUE,
                             partial = NULL, write_to_disk = FALSE,
                             keep_in_mem = TRUE, retries = 2){

  # checks
  check_args_fetch_msg_header(msg_id, by, fields, negate_fields, peek,
                              partial, write_to_disk, keep_in_mem, retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  ## forcing server url to the format we need -- removing unnecessary final slashe(s)
  #.. and doing URL encoding
  url <- utils::URLencode(gsub("/+$", "", IMAP_conn$imapconf$url))

  # isolating the handle
  h <- IMAP_conn$imapconf$conn_handle

  if (!is.null(partial)) {
    warning("Setting a partial interval of characters may strip your results.")
  }

  msg_list <- loop_fetch_msg_header(url, msg_id, by, fields,
                                    negate_fields, peek, partial, write_to_disk,
                                    keep_in_mem, retries, handle = h)

  # attr(msg_list, which = 'mbox') = new_imapconf$mbox


  # handle sanitizing
  rm(h)
  return(msg_list)

}
