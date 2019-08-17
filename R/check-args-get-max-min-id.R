#' @title Max/Min Message Arguments Check
#'
#' @description Internal helper function for checking the arguments
#'     used in \link{get_max_id} and \link{get_min_id}  functions.
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
#'     servers. Default is \code{NULL}.
#' @param retries Number of attempts to connect and execute the command. Default
#'     is \code{2}.
#'
#' @return \code{NULL} if arguments are correct.
#'
#' @family miscellaneous helper
#' @family check args
#'
#' @keywords internal
#'
check_args_get_max_min_id <- function(imapconf, by, flag, retries) {

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
    msg='"flag" argument must be a character. See flag_options().')

  assertthat::assert_that(
    is.numeric(retries),
    assertthat::validate_that(retries >= 1),
    msg='"retries" must be an integer equal or greater than 1.')

  if (retries%%1 != 0) {
    warning('only the integer part of "retries" will be used.')
  }

  return(NULL)
}
