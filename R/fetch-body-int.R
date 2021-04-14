#' Fetch message body (message's full content)
#' @param msg_id A \code{numeric vector} containing one or more message ids.
#' @param use_uid Default is \code{FALSE}. In this case, results will be
#'   presented as message's sequence numbers. A message sequence number is a
#'   message's relative position to the oldest message in the mailbox. It may
#'   change after deleting or moving messages. If a message is deleted,
#'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
#'   command will be performed using the \code{"UID"} or unique identifier,
#'   and results are presented as such. UIDs are always the same during the
#'   life cycle of a message.
#' @param mime_level An \code{integer} specifying MIME multipart to fetch from
#'   the message's body. Default is \code{NULL}, which retrieves the full body content.
#' @param peek If \code{TRUE}, it does not mark messages as "read" after
#'   fetching. Default is \code{TRUE}.
#' @param partial \code{NULL} or a character string with format
#'   "startchar.endchar" indicating the size (in characters) of a message slice
#'   to fetch. Default is \code{NULL}, which will fetch the full specified content.
#' @param write_to_disk If \code{TRUE}, writes the fetch content of each message
#'   to a text file in a local folder inside the working directory, also
#'   returning the results with \code{invisible()}. Default is \code{FALSE}.
#' @param keep_in_mem If \code{TRUE}, keeps a copy of each fetch result while
#'   the operation is being performed with \code{write_to_disk = TRUE}. Default
#'   is \code{FALSE}, and it can only be set \code{TRUE} when
#'   \code{write_to_disk = TRUE}.
#' @param mute A \code{logical}. It is only effective when \code{write_to_disk = TRUE}
#'   and \code{keep_in_mem = FALSE}. It provides a confirmation message if the
#'   command is successfully executed. Default is \code{FALSE}.
#' @param retries Number of attempts to connect and execute the command. Default
#'   is \code{1}.
#' @noRd
fetch_body_int <- function(self, msg_id, use_uid, mime_level, peek, partial, write_to_disk,
                           keep_in_mem, mute, retries) {

  if (isFALSE(keep_in_mem)) { # only for the fetch_body and fetch_text
    warning('"keep_in_mem = FALSE" will not alow you to use list_attachments() or get_attachments() after fetching.')
  }

  if (!is.null(mime_level)) { # only for the fetch_body
    assertthat::assert_that(
      is.integer(mime_level),
      msg='"mime_level" must be an integer.')
  }

  #check
  check_args(msg_id = msg_id, use_uid = use_uid, peek = peek, partial = partial,
             write_to_disk = write_to_disk, keep_in_mem = keep_in_mem,
             mute = mute, retries = retries)

  # peek
  if (isTRUE(peek)) {
    body_string = " BODY.PEEK[MIME.level]"
  } else {
    body_string = " BODY[MIME.level]"
  }

  if (!is.null(mime_level)) {
    body_string <- gsub('MIME.level', mime_level, body_string)
  } else {
    body_string <- gsub('MIME.level', '', body_string)
  }

  # partial
  if (!is.null(partial)) {
    partial_string = paste0("<", partial, ">")
  } else {
    partial_string = NULL
  }

  # use_uid
  if (isTRUE(use_uid)) {
    use_uid_string = "UID "
  } else {
    use_uid_string = NULL
  }

  fetch_request <- paste0(use_uid_string, "FETCH ", "#", body_string, partial_string) # "#" serves as a space holder for the msg's ids

  fetch_type = "body"
  # loop exec
  msg_list <- execute_fetch_loop(self = self, msg_id = msg_id, fetch_request = fetch_request,
                                 use_uid = use_uid, write_to_disk = write_to_disk,
                                 keep_in_mem = keep_in_mem, retries = retries,
                                 fetch_type = fetch_type)


  if (isFALSE(keep_in_mem)) {

    rm(msg_list)

    if (!mute) {
      cat(paste0("\n::mRpostman: fetch operation is complete.\n")) # v0.3.2
      # using the folder name without any transformation
    }

    return(TRUE)

  } else {
    return(msg_list)
  }

}
