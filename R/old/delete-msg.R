#' @title Delete Messages
#'
#' @description Deletes messages from a mailbox.
#'
#' @inheritParams check_args_delete_msg
#'
#' @return An (invisible) \code{vector} containing the previously inputed
#'     message ids (parameter \code{msg_id}).
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
#' select_folder(name = "INBOX")
#'
#' # deleting based on search results
#' search_before(date_char = "10-may-2012", by = "UID") %>%
#'     delete_msg()
#'
#'
#' # deleting a specific msg_id without a previous search
#' select_folder(name = "TAM")
#' delete_msg(msg_id = 66128, by = "UID")
#'
#' }
#'
#' @export
#'
delete_msg <- function(msg_id, by = "MSN", retries = 2) {

  check_args_delete_msg(msg_id, by, retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  ## forcing server url to the format we need -- removing unnecessary final slashe(s)
  #.. and doing URL encoding
  url <- utils::URLencode(gsub("/+$", "", IMAP_conn$imapconf$url))

  # isolating the handle
  h <- IMAP_conn$imapconf$conn_handle

  msg_id_string = paste0(msg_id, collapse = ",")

  # setting as deleted first
  if (by == "UID") {

    tryCatch({
      curl::handle_setopt(
        handle = h,
        customrequest = paste0("UID STORE ", msg_id_string, " FLAGS (\\Deleted)"))
    }, error = function(e){
      stop("The connection handle is dead. Please, configure a new IMAP connection with configure_imap().")
    })

  } else {

    tryCatch({
      curl::handle_setopt(
        handle = h,
        customrequest = paste0("STORE ", msg_id_string, " FLAGS (\\Deleted)"))
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

  # final_output <- list("imapconf" = imapconf, "msg_id" = msg_id) # 2nd arg bit different from others
  # will allow users to pipe more operations after adding flags
  invisible(0L) #ou
  # invisible(msg_id)


}
