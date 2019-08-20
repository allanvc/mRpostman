#' @title Delete Messages
#'
#' @description Deletes messages from a mailbox.
#'
#' @inheritParams check_args_delete_msg
#'
#' @return An (invisible) \code{list} of length \code{2} containing the
#'     \code{imapconf} object and the previously inputed message ids
#'     (parameter \code{msg_id}).
#'
#' @family miscellaneous
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
#'
#' # deleting based on search results
#' result1 <- imapconf %>%
#'     select_mailbox(mbox = "TAM") %>%
#'     search_before(date_char = "10-may-2012", by = "UID") %$% #modified pipe operator - pass two argg
#'     delete_msg(imapconf = imapconf, msg_id = msg_id)
#'
#'
#' # deleting a specific msg_id without a previous search
#' result2 <- imapconf %>%
#'     select_mailbox(mbox = "INBOX") %>%
#'     delete_msg(msg_id = 66128)
#'
#' }
#'
#' @export
#'
delete_msg <- function(imapconf, msg_id, by = "MSN", retries = 2) {

  check_args_delete_msg(imapconf, msg_id, by, retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  # copying imapconf to return the original in the end
  new_imapconf = imapconf

  # config handle
  h <- config_handle(imapconf)

  msg_id_string = paste0(msg_id, collapse = ",")

  # setting as deleted first
  if (by == "UID") {
    curl::handle_setopt(
      handle = h,
      customrequest = paste0("UID STORE ", msg_id_string, " FLAGS (\\Deleted)"))

  } else {
    curl::handle_setopt(
      handle = h,
      customrequest = paste0("STORE ", msg_id_string, " FLAGS (\\Deleted)"))
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

  final_output <- list("imapconf" = imapconf, "msg_id" = msg_id) # 2nd arg bit different from others
  # will allow users to pipe more operations after adding flags
  invisible(final_output)


}
