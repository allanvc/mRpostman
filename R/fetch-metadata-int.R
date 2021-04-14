#' Fetch message's metadata
#' @param msg_id A \code{numeric vector} containing one or more message ids.
#' @param use_uid Default is \code{FALSE}. In this case, results will be
#'   presented as message's sequence numbers. A message sequence number is a
#'   message's relative position to the oldest message in the mailbox. It may
#'   change after deleting or moving messages. If a message is deleted,
#'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
#'   command will be performed using the \code{"UID"} or unique identifier,
#'   and results are presented as such. UIDs are always the same during the
#'   life cycle of a message.
#' @param attribute An optional \code{character vector} specifying one or more
#'   attributes of the metadata of a message to fetch. See \link{metadata_options}.
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
#'   and \code{keep_in_mem = FALSE}. It Provides a confirmation message if the
#'   command is successfully executed. Default is \code{FALSE}.
#' @param retries Number of attempts to connect and execute the command. Default
#'   is \code{1}.
#' @noRd
fetch_metadata_int <- function(self, msg_id, use_uid, attribute, write_to_disk,
                               keep_in_mem, mute, retries) {

  #check
  check_args(msg_id = msg_id, use_uid = use_uid, attribute = attribute,
             write_to_disk = write_to_disk, keep_in_mem = keep_in_mem,
             mute = mute, retries = retries)

  if (is.null(attribute)) {
    attribute <- metadata_options()
  }

  attribute <- paste0(attribute, collapse = " ")

  # use_uid
  if (isTRUE(use_uid)) {
    use_uid_string = "UID "
  } else {
    use_uid_string = NULL
  }

  fetch_request <- paste0(use_uid_string, "FETCH ", "#", " (", attribute, ")") # "#" serves as a place holder for the msg's ids

  # loop exec
  fetch_type = "metadata"
  msg_list <- execute_fetch_loop(self = self, msg_id = msg_id, fetch_request = fetch_request,
                                 use_uid = use_uid, write_to_disk = write_to_disk,
                                 keep_in_mem = keep_in_mem, retries = retries,
                                 fetch_type = fetch_type, metadata_attribute = attribute)


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
