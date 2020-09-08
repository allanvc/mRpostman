#' Fix stripped search results
#' @param response An \code{integer vector} containing message's ids from
#'   the search.
#' @noRd
fix_search_stripping <- function(response) {

  # sort numbers from response
  # when it is different from the original vector, fix that number
  # according to the number of digits of the next number to the right

  # it does not seem to work anymore. Gmail seems to changed the response
  # MS Exchange also informs that there is a stripping, but is not possible
  # to retrieve this message in the curl's current version

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

  return(as.integer(as.character(response)))
}
