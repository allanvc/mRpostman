#' @title Move Message Arguments Check
#'
#' @description Internal helper function for checking the arguments
#'     used in \link{move_msg} function.
#'
#' @param imapconf An object of class \code{imapconf} generated with
#'     \link{configure_imap} and with a \code{mbox} item added after a
#'     mailbox selection with \link{select_mailbox}.
#' @param msg_id A numeric vetor containing one or more messages ids.
#' @param by Which id (\code{MSN} or \code{UID}) to use when searching for
#'     messages. \code{"MSN"} or message squence number is a message's relative
#'     position to the older message in the mailbox. It may change after deleting
#'     or moving messages. For instance, if a message is deleted, message sequence
#'     numbers are reordered to fill the gap. \code{"UID"} or unique identifier
#'     is always the same during the life cycle of a message. Default is
#'     \code{"MSN"}.
#' @param to_mbox A character string specifying the mailbox to which messages
#'     will be moved to.
#' @param reselect_mbox If \code{TRUE}, calls \code{select_mailbox(mbox = to_mbox)}
#'     before returning the output. Default is \code{FALSE} for moving and
#'     copying operations, whereas it is \code{TRUE} for renaming mailboxes.
#' @param retries Number of attempts to connect and execute the command. Default
#'     is \code{2}.
#'
#' @return \code{NULL} if arguments are correct.
#'
#' @family miscellaneous helpers
#' @family check args
#'
#' @keywords internal
#'
check_args_move_msg <- function(imapconf, msg_id, by, to_mbox, retries) {

  # checks
  assertthat::assert_that(
    assertthat::validate_that(class(imapconf) == "imapconf"),
    msg='"imapconf" must be of class "imapconf". Use configIMAP() to create a valid "imapconf" object.')

  assertthat::assert_that(
    is.numeric(msg_id),
    msg='"msg_id" must be a numeric vector of equal or greater than 1.')

  assertthat::assert_that(
    any(
      msg_id != 0,
      !is.na(msg_id)
    ),  msg='"msg_id" cannot contain 0 or NA')
  # it will be important when dealing with pipes and receiving info from search___() functions
  #... when no msg was found

  assertthat::assert_that(
    any(
      by == "MSN",
      by == "UID"
    ),
    msg='"by" must be set as "MSN" or "UID".')

  assertthat::assert_that(
    is.character(to_mbox),
    msg='"to_mbox" must be of type character. See listMailboxes().')

  assertthat::assert_that(
    is.numeric(retries),
    assertthat::validate_that(retries >= 1),
    msg='"retries" must be an integer equal or greater than 1.')

  if (retries%%1 != 0) {
    warning('only the integer part of "retries" will be used.')
  }

  return(NULL)
}
