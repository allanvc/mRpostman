#' @title Expunge Messages
#'
#' @description Expunge a specific message (using specific UID) or all messages
#'     marked as "DELETED" in a mailbox.
#'
#' @inheritParams check_args_expunge
#'
#' @inherit select_folder return
#'
#' @family miscellaneous
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
#' # deleting a message using specific UID
#' delete_msg(msg_id = 71171, by = "UID")
#' expunge(specific_UID = 71171)
#'
#'
#' # expunge all messages marked as "DELETED" in INBOX
#' select_folder(name = "TAM")
#' expunge()
#'
#' }
#'
#' @export
#'
expunge <- function(specific_UID = NULL, retries = 2) {

  check_args_expunge(specific_UID, retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  ## forcing server url to the format we need -- removing unnecessary final slashe(s)
  #.. and doing URL encoding
  url <- utils::URLencode(gsub("/+$", "", IMAP_conn$imapconf$url))

  # isolating the handle
  h <- IMAP_conn$imapconf$conn_handle

  # setting as deleted first
  if (!is.null(specific_UID)) {
    UID_string = paste0(specific_UID, collapse = ",")

    tryCatch({
      curl::handle_setopt(
        handle = h,
        customrequest = paste0("UID EXPUNGE ", UID_string))
    }, error = function(e){
      stop("The connection handle is dead. Please, configure a new IMAP connection with configure_imap().")
    })

  } else {

    tryCatch({
      curl::handle_setopt(
        handle = h,
        customrequest = "EXPUNGE")
    }, error = function(e){
      stop("The connection handle is dead. Please, configure a new IMAP connection with configure_imap().")
    })

  }

  # REQUEST
  response <- tryCatch({
    curl::curl_fetch_memory(url, handle = h)
  }, error = function(e){
    # print(e$message)
    response_error_handling(e$message[1]) # returns NULL for operation timeout: try reconnection
  })

  if (is.null(response)) {

    # it is not necessary to select again
    count_retries = 1 #the first try was already counted
    # FORCE appending fresh_connect
    # curl::handle_setopt(handle = h, fresh_connect = TRUE)

    while (is.null(response) && count_retries < retries) {
      count_retries = count_retries + 1
      # REQUEST
      response <- tryCatch({
        curl::curl_fetch_memory(url, handle = h)
      }, error = function(e){
        # print(e$message)
        response_error_handling(e$message[1]) # returns NULL for operation timeout: try reconnection
      })
    }

    if (is.null(response)) {
      stop('Request error: the server returned an error.')

    }

  }

  # handle sanitizing
  rm(h)

  invisible(0L)

}
