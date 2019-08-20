#' @title Mailbox Selection
#'
#' @description Select a mailbox on the server for issuing further commands.
#'
#' @param imapconf A object of class \code{imapconf} generated with
#'     \link{configure_imap}.
#' @param mbox A string containing the name of an existing mailbox on the server.
#' @param retries Number of attempts to connect and execute the command. Default
#'     is \code{2}.
#'
#' @return The (invisible) \code{imaconf} object with the selected mailbox added to it.
#'
#' @family mailbox commands
#'
#' @examples
#' \dontrun{
#'
#' # configure IMAP
#' library(mRpostman)
#' imapconf <- configureIMAP(url="imaps://imap.gmail.com",
#'                           username="your_gmail_user",
#'                           password=rstudioapi::askForPassword()
#'                          )
#'
#' # Select INBOX
#' imapconf %>%
#'   select_mailbox(mbox = "INBOX")
#'
#' }
#' @export
#'
select_mailbox <- function(imapconf, mbox, retries = 2) {

  # check
  assertthat::assert_that(
    assertthat::validate_that(class(imapconf) == "imapconf"),
    msg='"imapconf" must be of class "imapconf". Use configIMAP() to create a valid "imapconf" object.')

  assertthat::assert_that(
    is.character(mbox),
    msg='"mbox" must be of type character. See listMailboxes().')

  assertthat::assert_that(
    is.numeric(retries),
    assertthat::validate_that(retries >= 1),
    msg='"retries" must be an integer equal or greater than 1.')

  if (retries%%1 != 0) {
    warning('only the integer part of "retries" will be used.')
  }

  # forcing retries as an integer
  retries <- as.integer(retries)

  # forcing mbox to imap server accepted format for SEARCH? command
  mbox <- gsub(" ", "%20", mbox)

  mbox_check = stringr::str_detect(string = mbox,
                      '^\\".*\\"$') # to know if select_mailbox is being called
  #..from inside a search function after a failed attempt
  # we want to know if we have already added quotes
  if (!isTRUE(mbox_check)) {
    mbox <- paste0('"', mbox, '"')
  }

  # forcing server url to the format we need
  # -- removing unnecesary final slashe(s)
  imapconf$url <- gsub("/+$","", imapconf$url)

  imapconf$url = utils::URLencode(imapconf$url)

  imapconf$mbox = mbox

  # config handle
  h <- config_handle(imapconf)

  # adding the SELECT mbox customrequest parameter to handle
  curl::handle_setopt(handle = h,
                      customrequest = paste0("SELECT ", imapconf$mbox))

  response <- tryCatch({
    curl::curl_fetch_memory(imapconf$url, handle = h)
  }, error = function(e) {
    return(NULL)
  })

  if(is.null(response)){

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

    if (is.null(response)) {
      stop('An error ocurred while connecting. Please check the following and/or try again:\n
           - your internet connection status;\n
           - if your imapconf options are valid;\n
           - the name of the Mailbox (argument "mbox").'
      )
    }

  }
  invisible(imapconf)

}
