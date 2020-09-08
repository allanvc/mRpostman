#' @title String Search Arguments Check
#'
#' @inherit check_args_search_date description return
#'
#' @param string A character string specifying the word or expression to
#'     search for in messages.
#' @param section A mandatory \code{character} string specifying in which
#'     Section of the message ("HEADER", "TEXT" or "BODY") to search for the
#'     string.
#' @param negate If \code{TRUE}, negates the search and seeks for
#'     "NOT search_criterion". Default is \code{FALSE}.
#' @param by Which id (\code{MSN} or \code{UID}) to use when searching for
#'     messages. \code{"MSN"} or message sequence number is a message's relative
#'     position to the oldest message in the mailbox. It may change after deleting
#'     or moving messages. For instance, if a message is deleted, sequence
#'     numbers are reordered to fill the gap. The \code{"UID"} or unique identifier
#'     is always the same during the life cycle of a message. Default is
#'     \code{"MSN"}.
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
check_args_search_string_in_section <- function(string, section, negate, by, esearch,
                                               retries) {

  # checks
  assertthat::assert_that(
    is.character(string),
    msg='"string" argument must be a character.')

  assertthat::assert_that(
    is.character(section),
    msg='"section" argument must be a character.')

  assertthat::assert_that(
    is.logical(negate),
    msg='"negate" must be a logical.')

  assertthat::assert_that(
    any(
      by == "MSN",
      by == "UID"
    ),
    msg='"by" must be set as "MSN" or "UID".')

  assertthat::assert_that(
    is.logical(esearch),
    msg='"esearch" must be a logical.')

  assertthat::assert_that(
    is.numeric(retries),
    assertthat::validate_that(retries >= 1),
    msg='"retries" must be an integer equal or greater than 1.')

  if (retries%%1 != 0) {
    warning('only the integer part of "retries" will be used.')
  }

  return(NULL)
}
