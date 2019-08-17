#' @title Messages Results Decoding
#'
#' @description Internal helper function for decoding base64 message results.
#'
#' @param msg A character string with a message content.
#'
#' @return A string with a decoded message, if it was base64 coded.
#'
#' @family fetch helper
#' @family cleaning
#'
#' @keywords internal
#'
decode_base64_text_if_needed <- function(msg) {
  # to be used inside fetch_msg_text() - Headers and full msgs are fine,
  #.. except for the text part in full msgs
  check_result <- stringr::str_detect(string = msg,
                                      pattern = "(^[\\-\\-]+)|(<!DOCTYPE)")

  if (isFALSE(check_result)) {
    msg_converted <- tryCatch({
      # opted for tryCatch to not interrupt in case of error when converting
      rawToChar(base64enc::base64decode(msg)) # ensuring correct encode
      }, error = function(e) {
        return(msg)
      })

  } else {
    msg_converted <- msg
  }

  return(msg_converted)
}
