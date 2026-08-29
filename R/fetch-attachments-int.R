#' Fetch messages' attachments
#' @param msg_id A \code{numeric vector} containing one or more message ids.
#' @param use_uid Default is \code{FALSE}. In this case, results will be
#'   presented as message's sequence numbers. A message sequence number is a
#'   message's relative position to the oldest message in the mailbox. It may
#'   change after deleting or moving messages. If a message is deleted,
#'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
#'   command will be performed using the \code{"UID"} or unique identifier,
#'   and results are presented as such. UIDs are always the same during the
#'   life cycle of a message.
#' @param content_disposition A \code{string} indicating which type of
#'   "Content-Disposition" attachments should be retrieved. The options are
#'   \code{both}, \code{attachment}, and \code{inline}. Default is
#'   \code{"both"}, which retrieves regular attachments ("Content-Disposition:
#'   attachment") and  inline attachments ("Content-Disposition: inline").
#' @param override A \code{logical}. If \code{TRUE}, overrides existent files
#'   containing the same name in the local directory. Default is \code{FALSE}.
#' @param mute A \code{logical}. If \code{TRUE}, mutes the confirmation message
#'   when the command is successfully executed. Default is \code{FALSE}.
#' @param as_is If \code{TRUE} then write out attachments without base64
#'   decoding. Default is \code{FALSE}.
#' @noRd
fetch_attachments_int <- function(self, msg_id, use_uid, content_disposition, override,
                                  mute, retries, as_is, local_dir = ".") {

  check_args(msg_id = msg_id, use_uid = use_uid, content_disposition = content_disposition,
             override = override, mute = mute, retries = retries)

  if (is.na(self$con_params$folder)) {
    stop('No folder previously selected.')
  }

  # since 2.3.0 the fetch is guided by the BODYSTRUCTURE the server reports:
  # exact part numbers, nested multiparts included, one BODY.PEEK[<part>]
  # per attachment, decoded according to the declared transfer encoding
  out <- fetch_attachment_parts_int(self, msg_id = msg_id, use_uid = use_uid,
                                    parts = NULL, local_dir = local_dir,
                                    override = override, mute = TRUE,
                                    retries = retries,
                                    content_disposition = content_disposition,
                                    as_is = as_is)

  if (!mute) {
    cat(paste0("\n::mRpostman: the fetch operation is complete.\n"))
  }

  invisible(TRUE)
}
