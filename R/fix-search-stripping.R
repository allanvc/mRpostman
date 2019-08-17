#' @title Fixing Stripped Search Results
#'
#' @description Fixes stripped results from search operations when server may
#'     have stripped some lines.
#'
#' @param response An integer \code{vector} containing message ids from search.
#'
#' @return An integer \code{vector}.
#'
#' @family search helper
#' @family fix results
#'
#' @keywords internal
#'
fix_search_stripping <- function(response) {

  # sort numbers from response
  # when it is different from the original vector, fix that number
  # according to the number of digits of the next number to the right

  sorted_response = sort(response)

  if (any(which(response != sorted_response))) {

    stripped_idx <- which(response != sorted_response)[[1]]

    stripped_number <- response[stripped_idx]

    nchar_stripped_number <- nchar(stripped_number)

    nchar_next_number <- nchar(response[stripped_idx+1])

    response[stripped_idx] <- substr(
      x = stripped_number,
      start = (nchar_stripped_number - nchar_next_number)+1,
      stop = nchar_stripped_number)

  }

  return(response)
}
