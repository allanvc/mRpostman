#' @title Expunge Message Arguments Check
#'
#' @description Internal helper function for checking the arguments
#'     used in \link{expunge} function.
#'
#' @param imapconf An object of class \code{imapconf} generated with
#'     \link{configure_imap} and with a \code{mbox} item added after a
#'     mailbox selection with \link{select_mailbox}.
#' @param specific_UID A numeric vector containing one or more messages UIDs, if
#'     specific messages should be expunged and not the whole mailbox.
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
check_args_expunge <- function(imapconf, specific_UID, retries) {

  # checks
  assertthat::assert_that(
    assertthat::validate_that(class(imapconf) == "imapconf"),
    msg='"imapconf" must be of class "imapconf". Use configIMAP() to create a valid "imapconf" object.')

  assertthat::assert_that(
    any(
      is.null(specific_UID),
      is.numeric(specific_UID)
    ), msg='"specific_UID" can be NULL or a numeric vector of size equal or greater than 1.')

  assertthat::assert_that(
    is.numeric(retries),
    assertthat::validate_that(retries >= 1),
    msg='"retries" must be an integer equal or greater than 1.')

  if (retries%%1 != 0) {
    warning('only the integer part of "retries" will be used.')
  }

  return(NULL)
}
