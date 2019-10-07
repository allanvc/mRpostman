#' @title Flag Search Arguments Check
#'
#' @inherit check_args_search_date description return
#'
#' @param imapconf An object of class \code{imapconf} generated with
#'     \link{configure_imap} and with a \code{mbox} item added after a
#'     mailbox selection with \link{select_mailbox}.
#' @param flag A string specifying the flag to be used for filtering messages.
#'     Use \link{flag_options} to list the common flags used by IMAP
#'     servers.
#' @param negate If \code{TRUE}, negates the search and seeks for
#'     "NOT search_criterion". Default is \code{FALSE}.
#' @param by Which id (\code{MSN} or \code{UID}) to use when searching for
#'     messages. \code{"MSN"} or message squence number is a message's relative
#'     position to the older message in the mailbox. It may change after deleting
#'     or moving messages. For instance, if a message is deleted, message sequence
#'     numbers are reordered to fill the gap. \code{"UID"} or unique identifier
#'     is always the same during the life cycle of a message. Default is
#'     \code{"MSN"}.
#' @param esearch A logical. Default is \code{FALSE}. If the IMAP server has
#'     \code{ESEARCH} capability, it can be used to optimize search results. It
#'     allows to condense results to message sets to cut down on transmission
#'     costs, e.g. \code{1:5} instead of writing ids individually such as
#'     \code{1,2,3,4,5}. It can be used along with buffersize to avoid results
#'     stripping. Check if your IMAP server supports \code{SEARCH} with
#'     \code{\link{list_server_capabilities}}.
#' @param return_imapconf A logical. If \code{TRUE}, the function returns a
#'     \code{list} of length \code{2}, containing the \code{imapconf} object
#'     (IMAP settings) and the search results as message ids (\code{"MSN"} or
#'     \code{"UID"}). If \code{FALSE}, returns only the message ids as a numeric
#'     vector. Default is \code{TRUE}.
#' @param retries Number of attempts to connect and execute the command. Default
#'     is \code{2}.
#'
#' @family search helper
#' @family check args
#'
#' @keywords internal
#'
check_args_search_flag <- function(imapconf, flag, negate, by, esearch,
                                  return_imapconf, retries) {

  # checks
  assertthat::assert_that(
    assertthat::validate_that(class(imapconf) == "imapconf"),
    msg='"imapconf" must be of class "imapconf". Use configIMAP() to create a valid "imapconf" object.')

  assertthat::assert_that(
    is.character(flag),
    msg='"flag" argument must be a character. See mRpostman::flag_options().')

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
    is.logical(return_imapconf)
    ,
    msg='"return_imapconf" must be a logical')

  assertthat::assert_that(
    is.numeric(retries),
    assertthat::validate_that(retries >= 1),
    msg='"retries" must be an integer equal or greater than 1.')

  if (retries%%1 != 0) {
    warning('only the integer part of "retries" will be used.')
  }

  return(NULL)
}
