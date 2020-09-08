#' Copy message(s) between the selected folder and another one (INTERNAL HELPER)
#' @param msg_id A \code{numeric vector} containing one or more message ids.
#' @param use_uid Default is \code{FALSE}. In this case, results will be
#'   presented as message's sequence numbers. A message sequence number is a
#'   message's relative position to the oldest message in the mailbox. It may
#'   change after deleting or moving messages. If a message is deleted,
#'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
#'   command will be performed using the \code{"UID"} or unique identifier,
#'   and results are presented as such. UIDs are always the same during the
#'   life cycle of a message.
#' @param to_folder A \code{character} string specifying the folder to which
#'   the messages will be copied.
#' @param reselect A logical. If \code{TRUE}, calls
#'   \href{#method-select_folder}{\code{ImapCon$select_folder(name = to_folder)}}
#'   under the hood before returning the output. Default is \code{TRUE}.
#' @param mute A \code{logical}. Provides a confirmation message if the
#'   command is successfully executed. Default is \code{FALSE}.
#' @param retries Number of attempts to connect and execute the command.
#'   Default is \code{1}.
#' @noRd
copy_msg_int <- function(self, msg_id, use_uid, to_folder, reselect, mute, retries) {

  check_args(msg_id = msg_id, use_uid = use_uid, to_folder = to_folder, reselect = reselect,
             mute = mute, retries = retries)

  # quoting to guarantee folder with more than one name
  folder <- adjust_folder_name(self$con_params$folder) # there is a reselection in the end
  to_folder2 <- adjust_folder_name(to_folder)

  # forcing retries as an integer
  retries <- as.integer(retries)

  url <- self$con_params$url

  h <- self$con_handle

  # prepare msg_id strings
  msg_string = paste0(msg_id, collapse = ",")

  # customrequest parameter
  if (isTRUE(use_uid)) {

    customrequest <- paste0("UID COPY ", msg_string, " ", to_folder2)

  } else {

    customrequest <- paste0("COPY ", msg_string, " ", to_folder2)

  }

  response <- execute_complementary_operations(self, url, handle = h, customrequest,
                                               retries)


  # reselecting
  if (isTRUE(reselect)) {
    # imapconf$folder = folder
    # select_folder(name = to_folder)
    reselected_folder <- select_folder_int(self, name = to_folder, mute = mute, retries = 0) # ok! v0.0.9
  } else {
    reselected_folder <- NULL
  }

  if (!mute) {
    if (self$con_params$verbose) {
      Sys.sleep(0.01)
    }
    cat(paste0("\n::mRpostman: message(s) copied")) # v0.3.2
    # using the folder name without any transformation
  }

  # will allow users to pipe more operations after adding flags
  return(list(msg_id = msg_id, folder = reselected_folder))

}
