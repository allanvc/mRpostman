#' @title Period Search Request
#'
#' @inherit define_searchrequest_date description return
#'
#' @param operation1 A character indicating the first argument
#'     (SINCE and variations) to the custom request.
#' @param operation2 A character indicating the first argument
#'     (BEFORE and variations) to the custom request.
#' @inheritParams check_args_search_period
#' @param handle A curl handle object.
#'
#' @family search helper
#' @family define searchrequest
#'
#' @keywords internal
#'
define_searchrequest_period <- function(operation1, since_date_char, operation2,
                                        before_date_char, negate, by, flag,
                                        esearch, handle) {

  # esearch
  if (isTRUE(esearch)) {
    esearch_string = "RETURN () "
  } else {
    esearch_string = NULL
  }

  # flag
  if (!is.null(flag)) {
    flag_string = paste0(flag, " ")
  } else {
    flag_string = NULL
  }

  # by
  if (by == "UID") {
    by_string = "UID "
  } else {
    by_string = NULL
  }

  # negate
  if (isTRUE(negate)) {
    negate_string = "NOT "
  } else {
    negate_string = NULL
  }

  curl::handle_setopt(
    handle = handle,
    customrequest = paste0(by_string, "SEARCH ", esearch_string, flag_string,
                           negate_string, "(", operation1, ' ', since_date_char, ' ',
                           operation2, ' ', before_date_char, ")"))

  return(handle)
}
