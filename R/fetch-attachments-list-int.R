#' Fetch attachments' list
#' @param msg_id A \code{numeric vector} containing one or more message ids.
#' @param use_uid Default is \code{FALSE}. In this case, results will be
#'   presented as message's sequence numbers. A message sequence number is a
#'   message's relative position to the oldest message in the mailbox. It may
#'   change after deleting or moving messages. If a message is deleted,
#'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
#'   command will be performed using the \code{"UID"} or unique identifier,
#'   and results are presented as such. UIDs are always the same during the
#'   life cycle of a message.
#' @param retries Number of attempts to connect and execute the command. Default
#'   is \code{1}.
#' @noRd
fetch_attachments_list_int <- function(self, msg_id, use_uid, retries) {

  #check
  check_args(msg_id = msg_id, use_uid = use_uid, retries = retries)

  # fetch metada
  metadata_list <- self$fetch_metadata(msg_id = msg_id,
                                       use_uid = use_uid,
                                       metadata = "BODYSTRUCTURE",
                                       write_to_disk = FALSE,
                                       keep_in_mem = TRUE,
                                       mute = TRUE, #not needed
                                       retries = retries)

  attachments_list <- list()

  for (i in seq_along(metadata_list)) {

    # i = 14

    id = names(metadata_list[i]) # doing this to conserve name attribute

    meta = metadata_list[[i]]

    if (has_attachment(meta, call_from = "fetch_attachments_list")) {

      # attachments
      attachments <- unlist(regmatches(meta, gregexpr('\\(\"(attachment|ATTACHMENT)\" \\(\"(filename|FILENAME)\" \"(.*?)\"', meta))) # ok.. GMAIL returns uppercase
      attachments <- gsub('\\(\"(attachment|ATTACHMENT)\" \\(\"(filename|FILENAME)\" \"', '', attachments)
      attachments <- gsub('\"', '', attachments) # literal... it is not a regular expression in this case

      # inliners
      inliners <- unlist(regmatches(meta, gregexpr('\\(\"(inline|INLINE)\" \\(\"(filename|FILENAME)\" \"(.*?)\"', meta))) # ok.. GMAIL returns uppercase
      inliners <- gsub('\\(\"(inline|INLINE)\" \\(\"(filename|FILENAME)\" \"', '', inliners)
      inliners <- gsub('\"', '', inliners) # literal... it is not a regular expression in this case

      if (identical(inliners, character(0))) {
        df_inline <- NULL
      } else {
        df_inline <-data.frame("filename" = inliners,
                               "content_disposition" = "inline",
                               # "encoding" = encodings,
                               row.names = NULL,
                               stringsAsFactors = FALSE)
      }

      if (identical(attachments, character(0))) {
        df_attachment <- NULL
      } else {
        df_attachment <-data.frame("filename" = attachments,
                                   "content_disposition" = "attachment",
                                   # "encoding" = encodings,
                                   row.names = NULL,
                                   stringsAsFactors = FALSE)
      }


      out_df <- rbind(df_inline, df_attachment)

      # out_df$filename <- gsub("\\?=\r\n\\s*|=\\?[A-Za-z0-9-]+\\?Q\\?|\\?=$","", out_df$filename)

      if (!is.null(out_df)) { #hypothetical case the has_attachments has failed

        out_df$filename <- gsub("\\?\\=\\s*|\\?=\r\n\\s*|=\\?[A-Za-z0-9-]+\\?Q\\?|\\?=$","", out_df$filename)

        # gsub("\\?\\=\\s*", "", out_df$filename)
        # "ending with"

        # substituting URI encoding of a dot (=2E|%2E) -- it happens with yandex mail in some cases
        # we opted for decoding only dots first to get the correct file extension part
        out_df$filename <- gsub("=2E|%2E",".", out_df$filename)

        forbiden_chars <- "[\\/:*?\"<>|]"
        out_df$filename <- gsub(forbiden_chars, "", out_df$filename)

        # standard URLdecoding:
        for (j in seq_along(out_df$filename)) {
          out_df$filename[j] <- tryCatch({
            utils::URLdecode(out_df$filename[j])
          }, warning = function(w) {
            out_df$filename[j]
          }, error = function(e) {
            out_df$filename[j]
          })
        }


        # binding regular attachments and inline attachments
        out <- list(out_df)

      } else {
        out <- list(NA)
      }


      names(out) <- id
      attachments_list <- c(attachments_list, out)

    } else { # when has_attachments returns FALSE

      out <- NA
      names(out) <- id
      attachments_list <- c(attachments_list, out)

    }

  }

  if (self$con_params$verbose) {
    Sys.sleep(0.01)  # wait for the end of the client-server conversation
  }
  return(attachments_list)

}
