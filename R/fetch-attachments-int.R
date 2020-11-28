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
#' @noRd
fetch_attachments_int <- function(self, msg_id, use_uid, content_disposition, override,
                                  mute, retries) {

  #check
  check_args(msg_id = msg_id, use_uid = use_uid, content_disposition = content_disposition,
             override = override, mute = mute, retries = retries)

  # previous folder selection checking
  if (is.na(self$con_params$folder)) { # a principio jah sera checado na metadata
    stop('No folder previously selected.')
  }
  # assertthat::assert_that(
  #   !is.na(self$folder),
  #   msg='No folder previously selected.')

  # preparing mbox part of the directory for saving
  # folder <- adjust_folder_name(self$folder)

  # mbox = attr(msg_list, "mbox")
  # self = con
  # msg_id = 4160
  # msg_id = 3977
  # msg_id = 60813
  # msg_id = 62168
  # msg_id = 83040
  # msg_id = 4239
  # msg_id = 141
  # use_uid = FALSE
  # use_uid = TRUE
  # retries = 1
  # content_disposition = "both"
  folder_clean = gsub("%20", "_", self$con_params$folder)
  forbiden_chars <- "[\\/:*?\"<>|]"
  folder_clean = gsub(forbiden_chars, "", folder_clean)

  # url <- "imaps://outlook.office365.com/"
  # url_folder <- unlist(regmatches(self$con_params$url, gregexpr("://(.*?)(/|.)$", self$con_params$url)))
  # url_folder = gsub(forbiden_chars, "", url_folder)

  # forcing retries as an integer
  retries <- as.integer(retries)

  # fetch metadata
  metadata_list <- self$fetch_metadata(msg_id = msg_id,
                                       use_uid = use_uid,
                                       attribute = "BODYSTRUCTURE",
                                       write_to_disk = FALSE,
                                       keep_in_mem = TRUE,
                                       mute = TRUE, #not needed
                                       retries = retries)

  # use_uid
  if (isTRUE(use_uid)) {
    use_uid_string = "UID "
  } else {
    use_uid_string = NULL
  }

  fetch_request <- paste0(use_uid_string, "FETCH ", "#", " BODY[level.MIME]") # "#" serves as a space holder for the msg's ids

  for (i in seq_along(metadata_list)) {

    # i = 1
    meta = metadata_list[[i]]

    id <- msg_id[i]

    if (isTRUE(use_uid)) {
      id_folder <- paste0("UID", id)
    } else {
      id_folder <- id
    }

    # print(id)
    df_meta_to_fetch <- extract_MIME_level_and_filenames(meta, use_uid)
    # i = 6

    if (!is.null(df_meta_to_fetch)) {

      if (content_disposition != "both") {
        df_meta_to_fetch <- df_meta_to_fetch[df_meta_to_fetch$content_disposition == content_disposition, ]
      }

      if (nrow(df_meta_to_fetch) > 0) {
        execute_attachment_fetch(self, id, id_folder, df_meta_to_fetch, fetch_request,
                                 folder_clean, content_disposition,
                                 override, retries)
      } # if not, do nothing

    } #if not, do nothing

  }

  if (!mute) {
    cat(paste0("\n::mRpostman: the fetch operation is complete.\n"))
  }

  return(TRUE)

}
