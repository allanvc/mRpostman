#' @title Examine Mailbox
#'
#' @description Retrieve the number of recent messages and the total number of
#'     messages in a mailbox.
#'
#' @param imapconf An object of class \code{imapconf} generated with
#'     \code{configure_imap} and with a \code{mbox} item added after a
#'     mailbox selection with \code{select_mailbox}
#' @param retries Number of attempts to connect and execute the command. Default
#'     is \code{2}.
#'
#' @return A \code{vector} containing the (\code{"EXISTS"}) and the
#'     (\code{"RECENT"}) number of messages in the selected mailbox.
#'
#' @family mailbox commands
#'
#' @examples
#' \dontrun{
#'
#' # configure IMAP
#' library(mRpostman)
#' imapconf <- configure_imap(url="imaps://your.imap.server.com",
#'                            username="your_username",
#'                            password=rstudioapi::askForPassword()
#'                           )
#'
#' # list mailboxes
#' results <- imapconf %>%
#'   list_mailboxes() %>%
#'   select_mailbox(mbox = "Sent") %>%
#'   examine_mailbox()
#'
#' }
#' @export
#'
examine_mailbox <- function(imapconf, retries = 2) {

  # checks
  assertthat::assert_that(
    assertthat::validate_that(class(imapconf) == "imapconf"),
    msg='"imapconf" must be of class "imapconf". Use configIMAP() to create a valid "imapconf" object.')

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

  # adding the EXAMINE mbox customrequest parameter
  curl::handle_setopt(handle = h, customrequest = paste0("EXAMINE ", imapconf$mbox))

  response <- tryCatch({
    curl::curl_fetch_memory(imapconf$url, handle = h)
  }, error = function(e) {
    return(NULL)
  })

  if (!is.null(response)) {
    response <- as.numeric(as.character(
      stringr::str_match_all(
      string = rawToChar(response$content),
      pattern = "([\\d]+) (EXISTS|RECENT)")[[1]][,2]
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
          pattern = "([\\d]+) (EXISTS|RECENT)")[[1]][,2]
      ))

    } else {
      stop('An error ocurred while connecting. Please check the following and/or try again:\n
           - your internet connection status;\n
           - if your imapconf options are valid;\n
           - the name of the Mailbox (argument "mbox").'
      )
    }

  }

  names(response) <- c("EXISTS", "RECENT")
  # handle sanitizing
  rm(h)
  return(response)

}
