#' @title Date Search Request
#'
#' @description Internal helper function for defining curl custom request to be
#'     used with each search function
#'
#' @param operation A character indicating which custom operation to execute.
#' @inheritParams check_args_search_date
#' @param handle A curl handle object.
#'
#' @return A curl handle object containing the custom request.
#'
#' @family search helper
#' @family define searchrequest
#'
#' @keywords internal
#'
define_searchrequest_date <- function(operation, date_char, negate, by, flag,
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
                           negate_string, operation, ' ', date_char))

  return(handle)
}
