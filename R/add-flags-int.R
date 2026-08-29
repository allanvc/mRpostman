#' Add Flags to message(s)
#' @param msg_id A \code{numeric vector} containing one or more message ids.
#' @param use_uid Default is \code{FALSE}. In this case, results will be
#'   presented as message's sequence numbers. A message sequence number is a
#'   message's relative position to the oldest message in the mailbox. It may
#'   change after deleting or moving messages. If a message is deleted,
#'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
#'   command will be performed using the \code{"UID"} or unique identifier,
#'   and results are presented as such. UIDs are always the same during the
#'   life cycle of a message.
#' @param flags_to_set A \code{character vector} containing one or more flag
#'   names to add to the specified message ids. If the flag to be set is a
#'   system flag, such as \code{\\SEEN}, \code{\\ANSWERED}, the name should be
#'   preceded by two backslashes \code{\\}.
#' @param mute A \code{logical}. Provides a confirmation message if the
#'   command is successfully executed. Default is \code{FALSE}.
#' @param retries Number of attempts to connect and execute the command.
#'   Default is \code{1}.
#' @noRd
add_flags_int <- function(self, msg_id, use_uid, flags_to_set, mute, retries,
                            unchanged_since = NULL) {

  check_args(msg_id = msg_id, use_uid = use_uid, flags_to_set = flags_to_set,
             mute = mute,
             retries = retries)

  retries <- as.integer(retries)

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  # prepare flag and msg_id strings
  flags_string <- paste(flags_to_set, collapse = " ")

  msg_string = paste0(msg_id, collapse = ",")

  # conditional STORE (CONDSTORE, RFC 7162): only messages whose modification
  # sequence is not greater than unchanged_since are updated; the others are
  # reported back in the [MODIFIED ...] response code
  if (!is.null(unchanged_since)) {
    assertthat::assert_that(is.numeric(unchanged_since), length(unchanged_since) == 1,
                            unchanged_since >= 0,
                            msg='"unchanged_since" must be NULL or a single non-negative number.')
    assert_capability(self, "CONDSTORE", command = "add_flags(unchanged_since = ...)",
                      rfc = "RFC 7162", retries = retries)
    cond_string <- paste0(" (UNCHANGEDSINCE ", format(unchanged_since, scientific = FALSE), ")")
  } else {
    cond_string <- ""
  }

  # setting customrequest
  if (isTRUE(use_uid)) {

    customrequest <- paste0("UID STORE ", msg_string, cond_string, " +FLAGS ", "(", flags_string, ")")


  } else {

    customrequest <- paste0("STORE ", msg_string, cond_string, " +FLAGS ", "(", flags_string, ")")

  }

  response <- execute_complementary_operations(self, url, handle = h, customrequest,
                                               retries)

  # capture possible errors (in case of non-existent/allowed flags, curl does not assess the server response as an error)
  if (!is.null(response)) {
    no_reason <- find_no_reply(paste(rawToChar(response$headers),
                                     rawToChar(response$content), sep = "\r\n"))
    if (!is.na(no_reason)) {
      # a rejected STORE (e.g. a non-existent flag) is not an error for curl
      stop("The server rejected the STORE command: ", no_reason, call. = FALSE)
    }
  }

  if (!mute) {
    if (self$con_params$verbose) {
      Sys.sleep(0.01)
    }
    cat(paste0("\n::mRpostman: flag(s) successfully added.")) # v0.3.2
    # using the folder name without any transformation
  }

  # handle sanitizing
  rm(h)

  # return(TRUE)
  if (!is.null(unchanged_since) && !is.null(response)) {
    attr(msg_id, "modified") <- parse_modified(
      paste(rawToChar(response$headers), rawToChar(response$content)))
  }
  return(msg_id)

}
