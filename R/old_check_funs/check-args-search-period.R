#' @title Period Search Arguments Check
#'
#' @inherit check_args_search_date description return
#'
#' @param since_date_char A character vector with format "DD-Mon-YYYY",
#'     e.g. "01-Apr-2019" indicating the start date (inclusive). We opted not to
#'     use objects of type "date", since IMAP servers like this not so common date
#'     format.
#' @param before_date_char A character vector with format "DD-Mon-YYYY",
#'     e.g. "01-Apr-2019" indicating the stop date (exclusive). We opted not to
#'     use objects of type "date", since IMAP server likes this not so common date
#'     format.
#' @param negate If \code{TRUE}, negates the search and seeks for
#'     "NOT search_criterion". Default is \code{FALSE}.
#' @param use_uid Default is \code{FALSE}. In this case, results will be
#'     presented as message's sequence numbers. A message sequence number is a
#'     message's relative position to the oldest message in the mailbox.
#'     It may change after deleting or moving messages. If a message
#'     is deleted, sequence numbers are reordered to fill the gap.
#'     If \code{TRUE}, the command will be performed using the \code{"UID"} or
#'     unique identifier, and results are presented as such. UIDs are always the
#'     same during the life cycle of a message.
#' @param flag Optional parameter that adds one or more flags as an additional
#'     filter to the search. Use \code{\link{list_flags}} to list the flags
#'     in a selected mail folder. Default is \code{NULL}.
#' @param esearch A logical. Default is \code{FALSE}. If the IMAP server has
#'     \code{ESEARCH} capability, it can be used to optimize search results. It
#'     allows to condense results to message sets to cut down on transmission
#'     costs, e.g. \code{1:5} instead of writing ids individually such as
#'     \code{1,2,3,4,5}. It can be used along with buffersize to avoid results
#'     stripping. Check if your IMAP server supports \code{SEARCH} with
#'     \code{\link{list_server_capabilities}}.
#' @param retries Number of attempts to connect and execute the command. Default
#'     is \code{2}.
#'
#' @family search helper
#' @family check args
#'
#' @keywords internal
#'
check_args_search_period <- function(since_date_char, before_date_char, negate,
                                     use_uid, flag, esearch, retries) {

  # checks
  assertthat::assert_that(
    stringr::str_detect(string = since_date_char,
                        pattern = "[\\d]{2}-[a-zA-Z]{3}-[\\d]{4}"),
    msg='"since_date_char" must be of type character with format DD-Mon-YYYY", e.g. "01-Apr-2019".')

  assertthat::assert_that(
    stringr::str_detect(string = before_date_char,
                        pattern = "[\\d]{2}-[a-zA-Z]{3}-[\\d]{4}"),
    msg='"before_date_char" must be of type character with format DD-Mon-YYYY", e.g. "01-Apr-2019".')

  assertthat::assert_that(
    is.logical(negate),
    msg='"negate" must be a logical.')

  # too much trouble dealing with dates and locales
  # we opted for a simple string in the same format imap's servers usually accept

  assertthat::assert_that(
    is.logical(use_uid),
    msg='"use_uid" must be a logical.')

  assertthat::assert_that(
    any(
      is.null(flag),
      is.character(flag)
    ),
    msg='"flag" argument must be NULL or a character.')

  assertthat::assert_that(
    is.logical(esearch),
    msg='"esearch" must be a logical.')

  assertthat::assert_that(
    is.numeric(retries),
    assertthat::validate_that(retries >= 0), # v0.9.0
    msg='"retries" must be an integer equal or greater than 0.')

  if (retries%%1 != 0) {
    warning('only the integer part of "retries" will be used.')
  }

  return(NULL)
}
