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

  mismatch <- which(response != sorted_response)

  # the fix needs the number to the right of the stripped one; when the
  # mismatch is the last element there is nothing to infer from - leave it
  if (length(mismatch) > 0 && mismatch[[1]] < length(response)) {

    stripped_idx <- mismatch[[1]]

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
