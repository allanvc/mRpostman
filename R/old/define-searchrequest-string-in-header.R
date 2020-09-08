#' @title String Search Request
#'
#' @inherit define_searchrequest_date description return
#'
#' @inheritParams check_args_search_string_in_header
#' @param handle A curl handle object.
#'
#' @family search helper
#' @family define searchrequest
#'
#' @keywords internal
#'
define_searchrequest_string_in_header <- function(string, field, negate, by, esearch,
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

  # field
  field_string = paste0(field, " ")

  # string
  string_string = paste0('"', string, '"')

  customrequest <- paste0(by_string, "SEARCH ", esearch_string,
                          negate_string, "(", field_string,
                          string_string, ")")

  tryCatch({
    curl::handle_setopt(
      handle = handle,
      customrequest = customrequest)
  }, error = function(e){
    stop("The connection handle is dead. Please, configure a new IMAP connection with configure_imap().")
  })

  return(c(handle = handle, customrequest = customrequest))
}
