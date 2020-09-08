#' Remove flag(s) of one or more messages
#' @param msg_id A \code{numeric vector} containing one or more message ids.
#' @param use_uid Default is \code{FALSE}. In this case, results will be
#'   presented as message's sequence numbers. A message sequence number is a
#'   message's relative position to the oldest message in the mailbox. It may
#'   change after deleting or moving messages. If a message is deleted,
#'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
#'   command will be performed using the \code{"UID"} or unique identifier,
#'   and results are presented as such. UIDs are always the same during the
#'   life cycle of a message.
#' @param flags_to_unset A \code{character vector} containing one ore more
#'   flag names that will be unset (removed). If the flag to be removed is an
#'   system flag, such as \code{\\SEEN}, \code{\\ANSWERED}, the name should be
#'   preceded by two backslashes \code{\\}.
#' @param mute A \code{logical}. Provides a confirmation message if the
#'   command is successfully executed. Default is \code{FALSE}.
#' @param retries Number of attempts to connect and execute the command.
#'   Default is \code{1}.
#' @noRd
remove_flags_int <- function(self, msg_id, use_uid, flags_to_unset, mute, retries) {

  check_args(msg_id = msg_id, use_uid, flags_to_unset = flags_to_unset,
             mute = mute,
             retries = retries)

  retries <- as.integer(retries)

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  # prepare flag and msg_id strings
  flags_string <- paste(flags_to_unset, collapse = " ")

  msg_string = paste0(msg_id, collapse = ",")


  # setting customrequest
  if (isTRUE(use_uid)) {

    customrequest <- paste0("UID STORE ", msg_string, " -FLAGS ", "(", flags_string, ")")


  } else {

    customrequest <- paste0("STORE ", msg_string, " -FLAGS ", "(", flags_string, ")")

  }

  response <- execute_complementary_operations(self, url, handle = h, customrequest,
                                               retries)

  # capture possible errors (in case of non-existent/allowed flags, curl does not assess the server response as an error)
  if (!is.null(response)) {
    error_check <- grepl(pattern = "^\\* NO ", x = rawToChar(response$headers)) # it will be on headers in this case
    if (isTRUE(error_check)) {
      stop(unlist(regmatches(rawToChar(response$headers),
                             regexec("\\* NO (.*?)\r\n",
                                     rawToChar(response$headers))))[[2]])
    } # if a flag "\flag" does not exist, it returns NULL (a regular error that we are used to)
  }

  if (!mute) {
    if (self$con_params$verbose) {
      Sys.sleep(0.01)
    }
    cat(paste0("\n::mRpostman: flag(s) successfuly removed.")) # v0.3.2
    # using the folder name without any transformation
  }

  # handle sanitizing
  rm(h)

  # return(TRUE)
  return(msg_id)

}
