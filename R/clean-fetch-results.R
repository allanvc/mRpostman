#' Clean message's results
#' @param msg_text A character string with a message content.
#' @noRd
clean_fetch_results <- function(msg_text, metadata_attribute = NULL, attachment_fetch = FALSE) {


  pattern1 = "\\* \\d+ FETCH.*BODY.*\\{\\d+\\}\r\n"
  # result <- stringr::str_remove(string = msg_text, pattern = pattern1)
  result <- gsub(pattern1, "", msg_text, ignore.case = TRUE, useBytes = TRUE)

  pattern2 = "\\)\r\n[A-Z]\\d+ OK Success\r\n"
  # result <- stringr::str_remove(string = result, pattern = pattern2)
  result <- gsub(pattern2, "", result, ignore.case = TRUE, useBytes = TRUE)

  pattern3 = "\\)\r\n[A-Z]\\d+ OK FETCH completed.\r\n" #MS Exchange and yandex
  # result <- stringr::str_remove(string = result, pattern = pattern3)
  result <- gsub(pattern3, "", result, ignore.case = TRUE, useBytes = TRUE)

  # attachments
  pattern4 = "\\)\r\n[A-Z]\\d+ OK FETCH completed\r\n" #MS Exchange
  # result <- stringr::str_remove(string = result, pattern = pattern3)
  result <- gsub(pattern4, "", result, ignore.case = TRUE, useBytes = TRUE)

  pattern5 = "\r\n UID \\d+ FLAGS \\(.*\\)" #MS Exchange # important for attachments fetching
  # result <- stringr::str_remove(string = result, pattern = pattern3)
  result <- gsub(pattern5, "", result, ignore.case = TRUE, useBytes = TRUE)

  # pattern5 = "\r\n \\d+ FLAGS \\(.*\\)" #MS Exchange
  # # result <- stringr::str_remove(string = result, pattern = pattern3)
  # result <- gsub(pattern5, "", result, ignore.case = TRUE)


  if (!is.null(metadata_attribute) && (!any(metadata_attribute == "UID" || metadata_attribute == "uid"))) {
    pattern6 = "UID \\d+$| UID \\d+$"
    # result <- stringr::str_remove(string = result, pattern = pattern2)
    result <- gsub(pattern6, "", result, ignore.case = TRUE, useBytes = TRUE)
  }

  pattern7 = "^\\* \\d+ FETCH \\(" # important for fetch_metadata()
  result <- gsub(pattern7, "", result, ignore.case = TRUE, useBytes = TRUE)

  if (isTRUE(attachment_fetch)) { # in order to not have problem with fetcH_body/text + get_attachments() combo
    pattern8 = "==\r\n FLAGS \\((.*?)\\)|=\r\n FLAGS \\((.*?)\\)" # important for fetch_attachments with use_uid = FALSE - it returns the msg flags after the attachment part on Office 365
    result <- gsub(pattern8, "", result, ignore.case = TRUE, useBytes = TRUE)
  }


  # note to self: base R functions like regmatches returns error in some cases
  # sub is too slow

  return(result)
}
