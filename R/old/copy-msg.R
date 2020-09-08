#' @title Copy Messages
#'
#' @description Copies messages form a mailbox to another.
#'
#' @inheritParams check_args_copy_msg
#'
#' @inherit search_before return
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
#' # copy search results from "Sent" to "INBOX"
#' select_folder(name = "Sent")
#' results <- search_before(date_char = "10-may-2012") %$% #exposition pipe operator - pass two argg
#'     copy_msg(msg_id = msg_id, to_folder = "INBOX")
#' }
#'
#' @export
#'
copy_msg <- function(msg_id, by = "MSN", to_folder, reselect = TRUE, retries = 2) {

  check_args_copy_msg(msg_id, by, to_folder, reselect, retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  # forcing server url to the format we need -- removing unnecessary final slashe(s)
  #.. and doing URL encoding
  url <- utils::URLencode(gsub("/+$", "", IMAP_conn$imapconf$url))

  # isolating the handle
  h <- IMAP_conn$imapconf$conn_handle

  # quoting to guarantee folder with more than one name
  folder <- adjust_folder_name(IMAP_conn$imaconf$folder) # there is a reselection in the end
  to_folder <- adjust_folder_name(to_folder)

  # prepare msg_id strings
  msg_id_string = paste0(msg_id, collapse = ",")

  # adding the SEARCH or UID SEARCH before date customrequest parameter
  if (by == "UID") {

    tryCatch({
      curl::handle_setopt(
        handle = h,
        customrequest = paste0("UID COPY ", msg_id_string, " ", to_folder))
    }, error = function(e){
      stop("The connection handle is dead. Please, configure a new IMAP connection with configure_imap().")
    })

  } else {

    tryCatch({
      curl::handle_setopt(
        handle = h,
        customrequest = paste0("COPY ", msg_id_string, " ", to_folder))
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

  # reselecting
  if (isTRUE(reselect)) {

    # imapconf$folder = folder
    select_folder(name = to_folder)
  }


  # final_output <- list("imapconf" = imapconf, "msg_id" = msg_id) # 2nd arg bit different from others
  # will allow users to pipe more operations after adding flags
  invisible(msg_id)


}
