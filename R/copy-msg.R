#' @title Copy Messages
#'
#' @description Copies messages form a mailbox to another.
#'
#' @inheritParams check_args_copy_msg
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
#' # copy search results from "Sent" to "INBOX"
#' results <- imapconf %>%
#'     select_mailbox(mbox = "Sent") %>%
#'     search_before(date_char = "10-may-2012") %$% #exposition pipe operator - pass two argg
#'     copy_msg(imapconf = imapconf, msg_id = msg_id, to_mbox = "INBOX")
#'
#' }
#'
#' @export
#'
copy_msg <- function(imapconf, msg_id, by = "MSN", to_mbox, reselect_mbox = FALSE,
                     retries = 2) {

  check_args_copy_msg(imapconf, msg_id, by, to_mbox, reselect_mbox, retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  # quoting to guarantee mbox with more than one name
  to_mbox <- paste0('"', to_mbox, '"')

  # copying imapconf to return the original at the end
  new_imapconf = imapconf

  # config handle
  h <- config_handle(new_imapconf)

  # prepare msg_id strings
  msg_id_string = paste0(msg_id, collapse = ",")

  # adding the SEARCH or UID SEARCH before date customrequest parameter
  if (by == "UID") {
    curl::handle_setopt(
      handle = h,
      customrequest = paste0("UID COPY ", msg_id_string, " ", to_mbox))

  } else {
    curl::handle_setopt(
      handle = h,
      customrequest = paste0("COPY ", msg_id_string, " ", to_mbox))
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

  # reselecting
  if (isTRUE(reselect_mbox)) {

    imapconf$mbox = to_mbox

    imapconf <- select_mailbox(imapconf = imapconf,
                                  mbox = imapconf$mbox)
  }


  final_output <- list("imapconf" = imapconf, "msg_id" = msg_id) # 2nd arg bit different from others
  # will allow users to pipe more operations after adding flags
  invisible(final_output)


}
