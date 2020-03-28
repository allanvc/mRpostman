#' @title List Attachments Arguments Check
#'
#' @description Internal helper function for listing attachments
#'
#' @param msg_list A \code{list} containing the full messages fetched from
#'     the server after using \code{\link{fetch_full_msg}}.
#'
#' @return \code{NULL} if arguments are correct.
#'
#' @family attachments helper
#' @family check args
#'
#' @keywords internal
#'
check_args_list_attachments <- function(msg_list) {

  assertthat::assert_that(
    is.list(msg_list),
    msg='"msg_list" must be a list returned by fetch_full_msg().')

  assertthat::assert_that(
    length(msg_list) != 0,
    msg='"msg_list" has length 0. There is no attachment to list.')

  return(NULL)
}
