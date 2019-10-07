#' @title Expunge Messages
#'
#' @description Expunge a specific message (using specific UID) or all messages
#'     marked as "DELETED" in a mailbox.
#'
#' @inheritParams check_args_expunge
#'
#' @return The (invisible) imapconf object that was previously inputed.
#'
#' @family miscellaneous
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
#' # deleting a message using specific UID
#' result1 <- imapconf %>%
#'     select_mailbox(mbox = "TAM") %>%
#'     delete_msg(msg_id = 71171, by = "UID") %$%
#'     expunge(imapconf = imapconf, specific_UID = msg_id)
#'
#'
#' # expunge all message smarked as "DELETED" in INBOX
#' result2 <- imapconf %>%
#'     select_mailbox(mbox = "INBOX") %>%
#'     expunge()
#'
#' }
#'
#' @export
#'
expunge <- function(imapconf, specific_UID = NULL, retries = 2) {

  check_args_expunge(imapconf, specific_UID, retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  # copying imapconf to return the original in the end
  new_imapconf = imapconf

  # config handle
  h <- config_handle(imapconf)


  # setting as deleted first
  if (!is.null(specific_UID)) {
    UID_string = paste0(specific_UID, collapse = ",")

    curl::handle_setopt(
      handle = h,
      customrequest = paste0("UID EXPUNGE ", UID_string))

  } else {
    curl::handle_setopt(
      handle = h,
      customrequest = "EXPUNGE")
  }

  # REQUEST
  response <- tryCatch({
    curl::curl_fetch_memory(new_imapconf$url, handle = h)
  }, error = function(e) {
    return(NULL)
  })

  if (is.null(response)) {

    # it is not necessary to select again
    count_retries = 1 #the first try was already counted
    # FORCE appending fresh_connect
    curl::handle_setopt(handle = h, fresh_connect = TRUE)

    while (is.null(response) && count_retries < retries) {
      count_retries = count_retries+1
      response <- tryCatch({
        curl::curl_fetch_memory(new_imapconf$url, handle = h)

      }, error = function(e) {
        return(NULL)
      })
    }

    if (is.null(response)) {
      stop('An error ocurred while connecting. Please check the following and/or try again:\n
           - your internet connection status;\n
           - if imapconf options are valid;\n
           - the name of the Mailbox (argument "mbox").'
      )

    }

  }

  # handle sanitizing
  rm(h)

  invisible(imapconf)

}
