#' @title Messages Results Cleaning
#'
#' @description Internal helper function for cleaning a message content in a
#'     string. It is used with fetch loop functions.
#'
#' @param msg A character string with a message content.
#'
#' @return A string with a cleaned message.
#'
#' @family fetch helper
#' @family cleaning
#'
#' @keywords internal
#'
clean_fetch_results <- function(msg) {


  pattern1="\\* \\d+ FETCH.*BODY.*\\{\\d+\\}\r\n"
  result <- stringr::str_remove(string = msg, pattern = pattern1)

  pattern2="\\)\r\n[A-Z]\\d+ OK Success\r\n"
  result <- stringr::str_remove(string = result, pattern = pattern2)

  # note to self: base R functions like regmatches return error in some cases
  # sub is too slow

  return(result)
}
