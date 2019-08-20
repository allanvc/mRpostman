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
#' imapconf <- configure_imap(url="imaps://imap.gmail.com",
#'                            username="your_gmail_user",
#'                            password=rstudioapi::askForPassword()
#'                           )
#' # fetching
#' results <- imapconf %>%
#'     select_mailbox(mbox = "TAM") %>%
#'     search_before(date_char = "10-may-2012", by = "UID") %$% #exposition pipe - two argg
#'     fetch_msg_header(imapconf = imapconf, msg_id = msg_id, fields = c("From", "To"))
#'
#' }
#'
#' @export
#'
fetch_msg_header <- function(imapconf, msg_id, by = "MSN", fields = NULL,
                           negate_fields = FALSE, peek = TRUE,
                           partial = NULL, write_to_disk = FALSE,
                           keep_in_mem = TRUE, retries = 2){

  # checks
  check_args_fetch_msg_header(imapconf, msg_id, by, fields, negate_fields, peek,
                            partial, write_to_disk, keep_in_mem, retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  # copying imapconf to return the original in the end
  new_imapconf = imapconf

  # config handle
  h <- config_handle(new_imapconf)

  msg_list <- loop_fetch_msg_header(new_imapconf, msg_id, by, fields,
                                  negate_fields, peek, partial, write_to_disk,
                                  keep_in_mem, retries, handle = h)


  if (!is.null(partial)) {
    warning("Setting a partial interval of characters may strip your results.")
  }

  # handle sanitizing
  rm(h)
  return(msg_list)

}
