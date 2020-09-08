#' @description Messages Results Decoding
#' @param msg A \code{character} string with a message content.
#' @noRd
decode_base64_text_if_needed <- function(msg) {
  # to be used inside fetch_msg_text() - Headers and full msgs are fine,
  #.. except for the text part in full msgs
  check_result <- stringr::str_detect(string = msg,
                                      pattern = "(^[\\-\\-]+)|(<!DOCTYPE)|(<body)")
  # there should be another cases
  # if we find other, we will add during updates

  if (isFALSE(check_result)) {
    msg_converted <- tryCatch({
      # opted for tryCatch to not interrupt in case of error when converting
      rawToChar(base64enc::base64decode(msg)) # ensuring correct encode
      }, error = function(e) {
        return(msg)
      })

    # trying to revert wrong transformation of simple text
    # finding patterns such as \xaeH\xa7\x82\xd8^n
    msg_converted <- tryCatch({
      nchar(msg_converted) #hack to determine if the decoding was due
      # if wrong, we will have a "invalid multibyte string"
      msg_converted
    }, error = function(e) {
      msg_converted = msg
      return(msg_converted)
    })


  } else {
    msg_converted <- msg

    # trying to revert wrong transformation of simple text
    # finding patterns such as \xaeH\xa7\x82\xd8^n
    msg_converted <- tryCatch({
      nchar(msg_converted) #hack to determine if the decoding was due
      msg_converted
    }, error = function(e) {
      msg_converted <- rawToChar(base64enc::base64decode(msg))
      return(msg_converted)
    })
  }

  return(msg_converted)
}
