#' @title Add/Replace Flags Arguments Check
#'
#' @description Internal helper function for checking the arguments
#'     used in \link{add_flags}, and \link{replace_flags} functions.
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
#' @param flags_to_set A character vector containing one ore more flag names to
#'     add to or replace in the specified message ids.
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
check_args_add_replace_flags <- function(imapconf, msg_id, by, flags_to_set,
                                         retries) {

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
    is.character(flags_to_set),
    msg='"flags_to_set" argument must be a character.')

  assertthat::assert_that(
    is.numeric(retries),
    assertthat::validate_that(retries >= 1),
    msg='"retries" must be an integer equal or greater than 1.')

  if (retries%%1 != 0) {
    warning('only the integer part of "retries" will be used.')
  }

  return(NULL)
}
