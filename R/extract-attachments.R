#' Extract attachments from already-fetched messages
#'
#' The offline half of the attachment family (since the 2026 refactoring):
#' takes the full text of messages already fetched with
#' \href{#method-fetch_body}{\code{ImapCon$fetch_body()}}, walks the MIME
#' multipart tree by its declared boundaries, and either reports the
#' attachments (\code{dest = NULL}) or writes them to disk. It replaces both
#' \code{list_attachments()} (reporting) and
#' \code{ImapCon$get_attachments()} (writing).
#' @param msg_list A \code{list} with the fetched messages, as returned by
#'   \href{#method-fetch_body}{\code{ImapCon$fetch_body()}} (the full
#'   message, whose headers declare the MIME boundaries). Text-only fetches
#'   (\code{fetch_text()}) lack those headers and cannot be walked reliably.
#' @param dest \code{NULL} (default) to only report: the return value is a
#'   named \code{list} with one \code{data.frame} per message (filename,
#'   content_disposition, type, size; zero rows when a message has no
#'   attachments). A directory path writes each message's attachments to
#'   \code{dest/<message id>/} and returns the same manifest invisibly.
#' @param content_disposition One of \code{"both"} (default),
#'   \code{"attachment"}, or \code{"inline"}.
#' @param override A \code{logical}. If \code{TRUE}, overrides existing files
#'   with the same name. Default is \code{FALSE}.
#' @param as_is If \code{TRUE}, writes payloads without decoding the transfer
#'   encoding. Default is \code{FALSE}.
#' @param mute A \code{logical}. If \code{TRUE}, mutes the confirmation
#'   message when writing. Default is \code{FALSE}.
#' @return A named \code{list} of \code{data.frame}s (see \code{dest}).
#' @family attachments
#' @examples
#' \dontrun{
#' out <- con$fetch_body(con$query(size > 1e6, use_uid = TRUE), use_uid = TRUE)
#' extract_attachments(out)                       # report only
#' extract_attachments(out, dest = "~/attachments") # write files
#' }
#' @export
extract_attachments <- function(msg_list, dest = NULL,
                                content_disposition = "both",
                                override = FALSE, as_is = FALSE,
                                mute = FALSE) {

  assertthat::assert_that(is.list(msg_list), length(msg_list) > 0,
                          msg = '"msg_list" must be a non-empty list of fetched messages.')
  assertthat::assert_that(
    is.character(content_disposition), length(content_disposition) == 1,
    content_disposition %in% c("both", "attachment", "inline"),
    msg = '"content_disposition" must be one of "both", "attachment", "inline".')

  forbiden_chars <- "[\\\\/:*?\"<>|]"
  manifest <- vector("list", length(msg_list))
  names(manifest) <- names(msg_list)

  for (i in seq_along(msg_list)) {
    id <- names(msg_list[i])
    id <- unlist(regmatches(id, regexec("UID\\d+|\\d+", id)))
    if (length(id) == 0) id <- as.character(i)

    parts <- mime_attachment_parts(split_mime_parts(msg_list[[i]]),
                                   content_disposition)

    filenames <- parts$filename
    if (nrow(parts) > 0) {
      filenames[is.na(filenames)] <- paste0("attachment_",
                                            which(is.na(filenames)), ".bin")
      filenames <- gsub(forbiden_chars, "", filenames)
    }
    manifest[[i]] <- data.frame(
      filename = if (nrow(parts) > 0) adjust_repeated_filenames(filenames) else character(0),
      content_disposition = if (nrow(parts) > 0) {
        ifelse(is.na(parts$disposition), "attachment", parts$disposition)
      } else character(0),
      type = if (nrow(parts) > 0) parts$type else character(0),
      size = if (nrow(parts) > 0) {
        vapply(parts$payload, length, integer(1))
      } else integer(0),
      stringsAsFactors = FALSE)

    if (!is.null(dest) && nrow(parts) > 0) {
      complete_path <- file.path(sub("/+$", "", dest), id)
      dir.create(complete_path, showWarnings = FALSE, recursive = TRUE)
      for (j in seq_len(nrow(parts))) {
        fname <- manifest[[i]]$filename[j]
        target <- if (isTRUE(override)) {
          file.path(complete_path, fname)
        } else {
          serialize_filename(sufix = fname, complete_path = complete_path)
        }
        if (isTRUE(as_is)) {
          writeBin(charToRaw(parts$text[j]), target)
        } else {
          writeBin(parts$payload[[j]], target)
        }
      }
    }
  }

  if (!is.null(dest)) {
    if (!mute) {
      message("mRpostman: attachment(s) extraction is complete.")
    }
    return(invisible(manifest))
  }
  manifest
}
