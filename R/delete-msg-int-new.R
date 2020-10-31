#' Delete AND expunge selected messages in the selected mail folder (INTERNAL HELPER)
#' @param msg_uid A \code{numeric vector} containing one or more messages UIDs.
#'   Only UIDs are allowed in this operation (note the "u" in msg_\emph{u}id).
#' @param use_uid Default is \code{FALSE}. In this case, the operation will
#'   be performed using message sequence numbers. A message sequence number
#'   is a message's relative position to the oldest message in a mail folder.
#'   It may change after deleting or moving messages. If a message is deleted,
#'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
#'   command will be performed using the \code{"UID"} or unique identifier.
#'   UIDs are always the same during the life cycle of a message in a mail folder.
#' @param mute A \code{logical}. If \code{TRUE}, the function silently
#'   executes the command without providing a confirmation message. Default is
#'   \code{FALSE}.
#' @param retries Number of attempts to connect and execute the command.
#'   Default is \code{1}.
#' @noRd
delete_msg_int <- function(self, msg_uid, use_uid, mute, retries = 1) {

  check_args(msg_uid = msg_uid, use_uid = use_uid, mute = mute, retries = retries) # we have to pass
  #.. the argg as arg = arg, so the check_args function can capture the names

  # forcing retries as an integer
  retries <- as.integer(retries)

  # self$imapconf$url <- utils::URLencode(gsub("/+$", "", self$url))
  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  # prepare msg_id strings
  msg_string = paste0(msg_id, collapse = ",")

  # setting customrequest
  if (isTRUE(use_uid)) {

    customrequest <- paste0("UID STORE ", msg_string, " FLAGS (\\Deleted)")

  } else {

    customrequest <- paste0("STORE ", msg_string, " FLAGS (\\Deleted)")

  }

  execute_complementary_operations(self, url, handle = h, customrequest, retries)

  expunge(self = self, msg_uid = msg_uid, mute = TRUE, retries = 0)

  # handle sanitizing
  rm(h)

  # final_output <- list("imapconf" = imapconf, "msg_id" = msg_id) # 2nd arg bit different from others
  if (!mute) {
    if (self$con_params$verbose) {
      Sys.sleep(0.01)
    }
    cat(paste0('\n::mRpostman: message(s) marked as "\\Deleted"')) # v0.3.2
    # using the folder name without any transformation
  }
  # will allow users to pipe more operations after adding flags
  return(TRUE) #ou
  # invisible(msg_id)


}
