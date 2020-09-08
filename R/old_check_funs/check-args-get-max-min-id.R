#' @title Max/Min Message Arguments Check
#'
#' @description Internal helper function for checking the arguments
#'     used in \code{\link{get_max_id}} and \code{\link{get_min_id}} functions.
#'
#' @param flag A string containing one or more flags to search for. Use
#'     \code{\link{list_flags}} to list the flags in a selected mail folder.
#' @param use_uid Default is \code{FALSE}. In this case, results will be
#'     presented as message's sequence numbers. A message sequence number is a
#'     message's relative position to the oldest message in the mailbox.
#'     It may change after deleting or moving messages. If a message
#'     is deleted, sequence numbers are reordered to fill the gap.
#'     If \code{TRUE}, the command will be performed using the \code{"UID"} or
#'     unique identifier, and results are presented as such. UIDs are always the
#'     same during the life cycle of a message.
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
check_args_get_max_min_id <- function(flag, by, retries) {

  # checks
  assertthat::assert_that(
    is.character(flag),
    msg='"flag" argument must be a character.')

  assertthat::assert_that(
    any(
      by == "MSN",
      by == "UID"
    ),
    msg='"by" must be set as "MSN" or "UID".')

  assertthat::assert_that(
    is.numeric(retries),
    assertthat::validate_that(retries >= 1),
    msg='"retries" must be an integer equal or greater than 1.')

  if (retries%%1 != 0) {
    warning('only the integer part of "retries" will be used.')
  }

  return(NULL)
}
