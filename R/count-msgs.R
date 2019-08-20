#' @title Count Number of Messages
#'
#' @description Counts the number of messages with a respective flag in a mailbox.
#'
#' @param imapconf An object of class \code{imapconf} generated with
#'     \link{configure_imap} and with a \code{mbox} item added after a
#'     mailbox selection with \link{select_mailbox}.
#' @param by Which id (\code{MSN} or \code{UID}) to use when searching for
#'     messages. \code{"MSN"} or message squence number is a message's relative
#'     position to the older message in the mailbox. It may change after deleting
#'     or moving messages. For instance, if a message is deleted, message sequence
#'     numbers are reordered to fill the gap. \code{"UID"} or unique identifier
#'     is always the same during the life cycle of a message. Default is
#'     \code{"MSN"}.
#' @param flag A string specifying the flag to be used for filtering messages.
#'     Use \link{flag_options} to list the common flags used by IMAP
#'     servers.
#' @param retries Number of attempts to connect and execute the command. Default
#'     is \code{2}.
#'
#' @return A numeric \code{vector} of length \code{1} containing the number of
#'     messages that meet the specified criteria.
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
#' results <- imapconf %>%
#'     select_mailbox(mbox = "INBOX") %>%
#'     count_msgs(by = "UID", flag = "SEEN")
#'
#' }
#'
#' @export
#'
count_msgs <- function(imapconf, by = "MSN", flag, retries = 2) {

  # checks
  assertthat::assert_that(
    assertthat::validate_that(class(imapconf) == "imapconf"),
    msg='"imapconf" must be of class "imapconf". Use configIMAP() to create a valid "imapconf" object.')

  assertthat::assert_that(
    any(
      by == "MSN",
      by == "UID"
    ),
    msg='"by" must be set as "MSN" or "UID".')

  assertthat::assert_that(
    is.character(flag),
    msg='"flag" argument must be a character. See mRpostman::flag_options().')

  assertthat::assert_that(
    is.numeric(retries),
    assertthat::validate_that(retries >= 1),
    msg='"retries" must be an integer equal or greater than 1.')

  if (retries%%1 != 0) {
    warning('only the integer part of "retries" will be used.')
  }

  # forcing retries as an integer
  retries <- as.integer(retries)

  # config handle
  h <- config_handle(imapconf)

  # adding the SEARCH id RETURN COUNT customrequest
  if (by == "UID") {
    curl::handle_setopt(
      handle = h,
      customrequest = paste0("UID SEARCH RETURN (COUNT) ", flag))
  } else {
    curl::handle_setopt(
      handle = h,
      customrequest = paste0("SEARCH RETURN (COUNT) ", flag))
  }


  response <- tryCatch({
    curl::curl_fetch_memory(imapconf$url, handle = h)
  }, error = function(e) {
    return(NULL)
  })

  if (!is.null(response)) {
    response <- as.numeric(as.character(
      stringr::str_match_all(
        string = rawToChar(response$content),
        pattern = "COUNT ([\\d]+)\r\n")[[1]][,2]
    ))


  } else { # it is not necessary to select again
    count_retries = 1 #the first try was already counted
    # FORCE appending fresh_connect
    curl::handle_setopt(handle = h, fresh_connect = TRUE)

    while (is.null(response) && count_retries < retries) {
      count_retries = count_retries+1
      response <- tryCatch({
        curl::curl_fetch_memory(imapconf$url, handle = h)

      }, error = function(e) {
        return(NULL)
      })
    }

    if (!is.null(response)) {
      response <- as.numeric(as.character(
        stringr::str_match_all(
          string = rawToChar(response$content),
          pattern = "COUNT ([\\d]+)\r\n")[[1]][,2]
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

  names(response) <- c("COUNT")
  # handle sanitizing
  rm(h)
  return(response)

}
