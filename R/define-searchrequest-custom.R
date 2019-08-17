#' @title Custom Search Request
#'
#' @inherit define_searchrequest_date description return
#'
#' @inheritParams check_args_custom_search
#' @param handle A curl handle object.
#'
#' @return A curl handle object containing the custom request.
#'
##' @family search helper
#' @family define searchrequest
#'
#' @keywords internal
#'
define_searchrequest_custom <- function(custom_request, negate, by, esearch,
                                        handle) {

  # esearch
  if (isTRUE(esearch)) {
    esearch_string = "RETURN () "
  } else {
    esearch_string = NULL
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
    customrequest = paste0(by_string, "SEARCH ", esearch_string, custom_request))

  return(handle)

}
