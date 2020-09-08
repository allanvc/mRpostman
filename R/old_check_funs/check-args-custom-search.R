#' @title Custom Search Arguments Check
#'
#' @inherit check_args_search_date description return
#'
#' @param request A string directly specifying what to search or
#'     constructed by a combination of operators helper functions \code{\link{OR}}
#'     and \code{\link{AND}}, and criteria helper functions such as \code{\link{before}},
#'     \code{\link{since}}, \code{\link{on}}, \code{\link{sent_before}}, \code{\link{sent_since}},
#'     \code{\link{sent_on}}, \code{\link{flag}}, \code{\link{string}}, \code{\link{smaller_than}},
#'     \code{\link{larger_than}}, \code{\link{younger_than}}, or \code{\link{younger_than}}.
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
#' @param esearch A logical. Default is \code{FALSE}. If the IMAP server has
#'     \code{ESEARCH} capability, it can be used to optimize search results. It
#'     allows to condense results to cut down on transmission costs, e.g.
#'     \code{1:5} instead of writing ids individually such as \code{1,2,3,4,5}.
#'     It can be used along with buffersize to avoid results stripping.
#'     Check if your IMAP server supports \code{SEARCH} with \code{\link{list_server_capabilities}}.
#' @param retries Number of attempts to connect and execute the command. Default
#'     is \code{2}.
#'
#' @family search helper
#' @family check args
#'
#' @keywords internal
#'
check_args_custom_search <- function(request, negate, use_uid, esearch, retries) {

  # checks
  assertthat::assert_that(
    is.logical(negate),
    msg='"negate" must be a logical.')

  assertthat::assert_that(
    is.logical(use_uid),
    msg='"use_uid" must be a logical.')

  # assertthat::assert_that(
  #   any(
  #     by == "MSN",
  #     by == "UID"
  #   ),
  #   msg='"by" must be set as "MSN" or "UID".')

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
