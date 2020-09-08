#' Fetch message header
#' @param msg_id A \code{numeric vector} containing one or more message ids.
#' @param use_uid Default is \code{FALSE}. In this case, results will be
#'   presented as message's sequence numbers. A message sequence number is a
#'   message's relative position to the oldest message in the mailbox. It may
#'   change after deleting or moving messages. If a message is deleted,
#'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
#'   command will be performed using the \code{"UID"} or unique identifier,
#'   and results are presented as such. UIDs are always the same during the
#'   life cycle of a message.
#' @param fields An optional \code{character vector} specifying which field(s)
#'   will be fetched from the message's header. If none is specified, it will
#'   fetch the full header.
#' @param negate_fields If \code{TRUE}, negates the operation and seeks for
#'   "NOT in the field". Default is \code{FALSE}.
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
fetch_header_int <- function(self, msg_id, use_uid, fields, negate_fields,
                             peek, partial, write_to_disk, keep_in_mem,
                             mute, retries){

  #check
  check_args(msg_id = msg_id, use_uid = use_uid, fields = fields, negate_fields = negate_fields,
             peek = peek, partial = partial, write_to_disk = write_to_disk,
             keep_in_mem = keep_in_mem, mute = mute, retries = retries)

  # header string
  if (!is.null(fields)) {
    fields_string = paste0(fields, collapse = " ")

    if (isTRUE(negate_fields)) {
      header_string = paste0("[HEADER.FIELDS.NOT (", fields_string, ")]")

    } else {
      header_string = paste0("[HEADER.FIELDS (", fields_string, ")]")

    }

  } else {
    header_string = paste0("[HEADER]")

  }

  # body_string
  if (isTRUE(peek)) {
    body_string = paste0(" BODY.PEEK", header_string)
  } else {
    body_string = paste0(" BODY", header_string)
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

  fetch_request <- paste0(use_uid_string, "FETCH ", "#", body_string, partial_string) # "#" serves as a place holder for the msg's ids

  # loop exec
  fetch_type = "header"
  msg_list <- execute_fetch_loop(self = self, msg_id = msg_id, fetch_request = fetch_request,
                                 use_uid = use_uid, write_to_disk = write_to_disk,
                                 keep_in_mem = keep_in_mem, retries = retries,
                                 fetch_type = fetch_type)


  if (isFALSE(keep_in_mem)) {

    rm(msg_list)

    if (!mute) {
      cat(paste0("\n::mRpostman: the fetch operation is complete.\n")) # v0.3.2
      # using the folder name without any transformation
    }

    return(TRUE)

  } else {
    return(msg_list)
  }

}
