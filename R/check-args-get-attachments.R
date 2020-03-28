#' @title Get Attachments Arguments Check
#'
#' @description Internal helper function for getting attachments
#'
#' @param msg_list A \code{list} containing the full messages fetched from
#'     the server after using \code{\link{fetch_full_msg}}.
#' @param content_disposition A \code{string} indicating which type of
#'     "Content-Disposition" attachments should be retireved. Default is
#'     \code{"both"}, which retireves regular attachments
#'     ("Content-Disposition: attachment") and  inline attachments
#'     ("Content-Disposition: inline").
#'
#' @return \code{NULL} if arguments are correct.
#'
#' @family attachments helper
#' @family check args
#'
#' @keywords internal
#'
check_args_get_attachments <- function(msg_list, content_disposition) {

  assertthat::assert_that(
    is.list(msg_list),
    msg='"msg_list" must be a list returned by fetch_full_msg().')

  assertthat::assert_that(
    length(msg_list) != 0,
    msg='"msg_list" has length 0. There is no attachment to get.')

  assertthat::assert_that(
    any(
      content_disposition == "both",
      content_disposition == "attachment",
      content_disposition == "inline"
    ),
    msg='"content_disposition" must be set as "both", "attachment" or "inline".')

  return(NULL)
}
