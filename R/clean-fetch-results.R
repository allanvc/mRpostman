#' Clean message's results
#' @param msg_text A character string with a message content.
#' @noRd
clean_fetch_results <- function(msg_text) {


  pattern1 = "\\* \\d+ FETCH.*BODY.*\\{\\d+\\}\r\n"
  # result <- stringr::str_remove(string = msg_text, pattern = pattern1)
  result <- gsub(pattern1, "", msg_text)

  pattern2 = "\\)\r\n[A-Z]\\d+ OK Success\r\n"
  # result <- stringr::str_remove(string = result, pattern = pattern2)
  result <- gsub(pattern2, "", result)

  pattern3 = "\\)\r\n[A-Z]\\d+ OK FETCH completed.\r\n" #MS Exchange
  # result <- stringr::str_remove(string = result, pattern = pattern3)
  result <- gsub(pattern3, "", result)

  # attachments
  pattern4 = "\\)\r\n[A-Z]\\d+ OK FETCH completed\r\n" #MS Exchange
  # result <- stringr::str_remove(string = result, pattern = pattern3)
  result <- gsub(pattern4, "", result)

  # note to self: base R functions like regmatches return error in some cases
  # sub is too slow

  return(result)
}
