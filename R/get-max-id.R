#' @title Get Maximum Message ID in a Mailbox
#'
#' @description Get the greater (maximum) message id in a Mailbox, considering
#'     messages with a specific flag.
#'
#' @inheritParams check_args_get_max_min_id
#'
#' @return A numeric \code{vector} of length \code{1} indicating the minimum id.
#'
#' @note \code{get_max_id} uses \code{ESEARCH} extension. Check if your server
#'     supports \code{ESEARCH} capability with \link{list_server_capabilities}.
#'
#' @family miscellaneous
#' @family execution
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
#' results <- imapconf %>%
#'     select_mailbox(mbox = "[Gmail]/Trash") %>%
#'     get_max_id(by = "UID", flag = "SEEN")
#'
#' }
#'
#' @export
#'
get_max_id <- function(imapconf, by = "MSN", flag, retries = 2) {

  check_args_get_max_min_id(imapconf, by, flag, retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  new_imapconf <- imapconf

  # config handle
  h <- config_handle(new_imapconf)

  # adding the SEARCH id RETURN COUNT customrequest
  if (by == "UID") {
    curl::handle_setopt(
      handle = h,
      customrequest = paste0("UID SEARCH RETURN (MAX) ", flag))
  } else {
    curl::handle_setopt(
      handle = h,
      customrequest = paste0("SEARCH RETURN (MAX) ", flag))
  }


  response <- tryCatch({
    curl::curl_fetch_memory(new_imapconf$url, handle = h)
  }, error = function(e) {
    return(NULL)
  })

  if (!is.null(response)) {
    response <- as.numeric(as.character(
      stringr::str_match_all(
        string = rawToChar(response$content),
        pattern = "MAX ([\\d]+)\r\n")[[1]][,2]
    ))


  } else { # it is not necessary to select again
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

    if (!is.null(response)) {
      response <- as.numeric(as.character(
        stringr::str_match_all(
          string = rawToChar(response$content),
          pattern = "MAX ([\\d]+)\r\n")[[1]][,2]
      ))

    } else {
      stop('An error ocurred while connecting. Please check the following and/or try again:\n
           - your internet connection status;\n
           - if your IMAP server supports ESEARCH CAPABILITY;\n
           - if imapconf options are valid;\n
           - the name of the Mailbox (argument "mbox").'
      )
    }

  }

  if (!(length(response) >  0)) {
    response = 0
  }

  names(response) <- c("MAX")
  # handle sanitizing
  rm(h)

  return(response)

}
