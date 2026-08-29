#' Extract attached file(s) from fetched message(s) (INTERNAL HELPER)
#' @param msg_list A \code{list} with the body or text content of the messages
#'   fetched with \href{#method-fetch_body}{\code{ImapCon$fetch_body()}} or
#'   \href{#method-fetch_text}{\code{ImapCon$fetch_text()}}.
#' @param content_disposition A \code{string} indicating which type of
#'   "Content-Disposition" attachments should be retrieved. Default is
#'   \code{"both"}, which retrieves regular attachments ("Content-Disposition:
#'   attachment") and  inline attachments ("Content-Disposition: inline").
#' @param override A \code{logical}. If \code{TRUE}, overrides existent files
#'   containing the same name in the local directory. Default is \code{FALSE}.
#' @param mute A \code{logical}. If \code{TRUE}, mutes the confirmation message
#'   when the command is successfully executed. Default is \code{FALSE}.
#' @param as_is If \code{TRUE} then write out attachments without base64
#'   decoding. Default is \code{FALSE}.
#' @noRd
get_attachments_int <- function(self, msg_list, content_disposition, override,
                                mute, as_is, local_dir = ".") {

  # previous folder selection checking
  if (is.na(self$con_params$folder)) {
    stop('No folder previously selected.')
  }

  check_args(msg_list = msg_list, content_disposition = content_disposition,
             override = override, mute = mute)

  forbiden_chars <- "[\\\\/:*?\"<>|]"
  folder_clean <- gsub("%20", "_", self$con_params$folder)
  folder_clean <- gsub(forbiden_chars, "", folder_clean)
  user_folder <- gsub(forbiden_chars, "", self$con_params$username)

  for (i in seq_along(msg_list)) {

    id <- names(msg_list[i])
    id <- unlist(regmatches(id, regexec("UID\\d+|\\d+", id)))
    msg <- msg_list[[i]]

    parts <- mime_attachment_parts(split_mime_parts(msg), content_disposition)

    if (nrow(parts) == 0) {
      message('No attachments with the specified "content_disposition" were found!')
      next
    }

    filenames <- parts$filename
    filenames[is.na(filenames)] <- paste0("attachment_", which(is.na(filenames)), ".bin")
    filenames <- gsub(forbiden_chars, "", filenames)
    adjusted_filenames <- adjust_repeated_filenames(filenames)

    complete_path <- paste0(sub("/+$", "", local_dir), "/", user_folder, "/",
                            folder_clean, "/", id)
    dir.create(path = complete_path, showWarnings = FALSE, recursive = TRUE)

    for (j in seq_len(nrow(parts))) {
      if (isTRUE(override)) {
        complete_path_with_filename <- paste0(complete_path, "/", adjusted_filenames[j])
      } else {
        complete_path_with_filename <- serialize_filename(
          sufix = adjusted_filenames[j], complete_path = complete_path)
      }
      if (isTRUE(as_is)) {
        writeBin(charToRaw(parts$text[j]), complete_path_with_filename)
      } else {
        writeBin(parts$payload[[j]], complete_path_with_filename)
      }
    }
  }

  if (!mute) {
    cat(paste0("\n::mRpostman: attachment(s) extraction is complete."))
  }

  return(TRUE)
}
