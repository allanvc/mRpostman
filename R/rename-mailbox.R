#' @title Rename Mailbox
#'
#' @description Rename a mailbox.
#'
#' @param imapconf An object of class \code{imapconf} generated with
#'     \code{configure_imap} and with a \code{mbox} item added after a
#'     mailbox selection with \link{select_mailbox}.
#' @param new_name A string containing the new name to be set.
#' @param reselect_mbox If \code{TRUE}, calls \code{select_mailbox(mbox = to_mbox)}
#'     before returning the output. Default is \code{FALSE} for moving and
#'     copying operations, whereas it is \code{TRUE} for renaming mailboxes.
#' @param retries Number of attempts to connect and execute the command. Default
#'     is \code{2}.
#'
#' @return An (invisible) \code{imaconf} object with the newer selected mailbox added
#'     to it. If \code{reselect_mbox} is \code{FALSE}, it returns the
#'     \code{imaconf} object with the former mailbox name.
#'
#' @family mailbox commands
#'
#' @examples
#' \dontrun{
#'
#' # configure IMAP
#' imapconf <- configure_imap(url="imaps://imap.gmail.com",
#'                           username="your_gmail_user",
#'                           password=rstudioapi::askForPassword()
#'                           )
#'
#' # rename mailbox "Sent"
#' imapconf %>%
#'   select_mailbox(mbox = "Sent") %>%
#'   rename_mailbox(new_name = "Sent Mail")
#'
#' }
#' @export
#'
rename_mailbox <- function(imapconf, new_name, reselect_mbox = TRUE,
                           retries = 2) {

  # checks
  assertthat::assert_that(
    assertthat::validate_that(class(imapconf) == "imapconf"),
    msg='"imapconf" must be of class "imapconf". Use configIMAP() to create a valid "imapconf" object.')

  assertthat::assert_that(
    is.character(new_name),
    msg='"new_name" must be of type character.')

  assertthat::assert_that(
    is.logical(reselect_mbox),
    msg='"reselect_mbox" must be a logical.')

  assertthat::assert_that(
    is.numeric(retries),
    assertthat::validate_that(retries >= 1),
    msg='"retries" must be an integer equal or greater than 1.')

  if (retries%%1 != 0) {
    warning('only the integer part of "retries" will be used.')
  }

  # forcing retries as an integer
  retries <- as.integer(retries)

  # quoting to guarantee mbox with more than one name
  new_name <- paste0('"', new_name, '"')

  # copying imapconf to return the original in the end
  new_imapconf = imapconf

  # config handle
  h <- config_handle(imapconf)

  # adding the EXAMINE mbox customrequest parameter
  curl::handle_setopt(handle = h, customrequest = paste0("RENAME ",
                                                         new_imapconf$mbox, " ",
                                                         new_name))

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

    new_imapconf$mbox = new_name

    new_imapconf <- select_mailbox(imapconf = new_imapconf,
                                  mbox = new_imapconf$mbox)

    invisible(new_imapconf)

  } else {

    invisible(imapconf)
  }

}
