#' Expunge the selected mail folder or specific message(s) (by UID) (INTERNAL HELPER)
#' @param msg_uid A \code{numeric vector} containing one or more messages UIDs.
#'   This operation does not allow sequence numbers.
#' @param mute A \code{logical}. Provides a confirmation message if the
#'   command is successfully executed. Default is \code{FALSE}.
#' @param retries Number of attempts to connect and execute the command.
#'   Default is \code{1}.
#' @noRd
expunge_int <- function(self, msg_uid, mute, retries) {

  # expunge only acepts UIDs if the user is to pass a specific message to expunge

  check_args(msg_uid = msg_uid, mute = mute, retries = retries)

  retries <- as.integer(retries)

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  # prepare msg_id strings
  msg_string = paste0(msg_uid, collapse = ",")

  # setting customrequest
  if (!is.null(msg_uid)) {

    customrequest <- paste0("UID EXPUNGE ", msg_string)

  } else {

    customrequest <- "EXPUNGE"

  }

  response <- execute_complementary_operations(self, url, handle = h, customrequest,
                                             retries) # special case here: use_uid = TRUE

  # handle sanitizing
  rm(h)

  # final_output <- list("imapconf" = imapconf, "msg_id" = msg_id) # 2nd arg bit different from others
  if (!mute) {
    if (self$con_params$verbose) {
      Sys.sleep(0.01)
    }
    cat(paste0("\n::mRpostman: expunge successfuly executed.")) # v0.3.2
    # using the folder name without any transformation
  }
  # will allow users to pipe more operations after adding flags
  return(TRUE) #ou
  # invisible(msg_id)

}
