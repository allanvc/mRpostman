#' @title Delete Message Arguments Check
#'
#' @description Internal helper function for checking the arguments
#'     used in \code{\link{delete_msg}} function.
#'
#' @param msg A \code{numeric vector} containing one or more messages ids.
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
check_args_delete_msg <- function(msg_id, by, retries) {

  # checks
  assertthat::assert_that(
    is.numeric(msg_id),
    msg='"msg_id" must be a numeric vector of equal or greater than 1.')

  assertthat::assert_that(
    msg_id != 0 & !is.na(msg_id),
    msg='"msg_id" must not be 0 or NA')
  # it will be important when dealing with pipes and receiving info from search___() functions
  #... when no msg was found

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
