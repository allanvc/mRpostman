#' Extract specific attachments Mime levels and the filenames of a message
#' @param meta An object of type \code{character} containing the BODYSTRUCTURE
#'   previously fetched messages via \code{fetch_metadata}.
#' @param use_uid Default is \code{FALSE}. In this case, results will be
#'   presented as message's sequence numbers. A message sequence number is a
#'   message's relative position to the oldest message in the mailbox. It may
#'   change after deleting or moving messages. If a message is deleted,
#'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
#'   command will be performed using the \code{"UID"} or unique identifier,
#'   and results are presented as such. UIDs are always the same during the
#'   life cycle of a message.
#' @noRd
extract_MIME_level_and_filenames <- function(meta, use_uid) {

  if(isTRUE(use_uid)) {
    meta <- gsub("FETCH \\(UID \\d+ BODYSTRUCTURE \\(", "", meta)
  } else {
    meta <- gsub("FETCH \\(BODYSTRUCTURE \\(", "", meta)
  }

  meta <- gsub('\\)$', "", meta)

  count_MIME_levels = 0
  paren_sum = 0
  j = 0

  attachments_MIME_level <- c()
  # attachment_init_char <- c()
  content_type <- c()

  while (j <= nchar(meta)) {
    # j = 360
    char <- substr(meta, j, j)

    if(char == "("){
      paren_sum = paren_sum + 1 # supposing it will never be negative

      if (grepl("attachment|ATTACHMENT", substr(meta, j, j + 15), ignore.case = TRUE)) {
        # attachment_init_char <- c(attachment_init_char, j + 1) # extrai desse ponto até o final da string, para depois extrair o nome
        content_type <- c(content_type, "attachment")
        attachments_MIME_level <- c(attachments_MIME_level, count_MIME_levels + 1)
      }

      if (grepl("inline|INLINE", substr(meta, j, j + 15), ignore.case = TRUE)) {
        # attachment_init_char <- c(attachment_init_char, j + 1) # extrai desse ponto até o final da string, para depois extrair o nome
        content_type <- c(content_type, "inline")
        attachments_MIME_level <- c(attachments_MIME_level, count_MIME_levels + 1)
      }

    } else if(char == ")"){
      paren_sum = paren_sum - 1
      if(paren_sum == 0) {
        count_MIME_levels = count_MIME_levels + 1
      }
    }

    j = j + 1

  }


  attachments_both <- unlist(
    regmatches(meta,
               gregexpr('\\(\"(attachment|ATTACHMENT|inline|INLINE)\" \\(\"(filename|FILENAME)\" \"(.*?)\"',
                        meta))) # ok.. GMAIL returns uppercase

  attachments_both <- gsub('\\(\"(attachment|ATTACHMENT|inline|INLINE)\" \\(\"(filename|FILENAME)\" \"', '',
                           attachments_both)
  attachments_both <- gsub('\"', '', attachments_both) # literal... it is not a regular expression in this case

  # fazer mais algumas limpezas nos nomes

  if (!identical(attachments_both, character(0))) {
    df_meta_to_fetch <- data.frame(filenames = attachments_both,
                                   MIME_level = attachments_MIME_level,
                                   content_disposition = content_type,
                                   row.names = NULL,
                                   stringsAsFactors = FALSE)

    df_meta_to_fetch$filenames <- gsub("\\?\\=\\s*|\\?=\r\n\\s*|=\\?[A-Za-z0-9-]+\\?Q\\?|\\?=$","",
                                      df_meta_to_fetch$filenames)

    # gsub("\\?\\=\\s*", "", out_df$filenames)
    # "ending with"

    # substituting URI encoding of a dot (=2E|%2E) -- it happens with yandex mail in some cases
    # we opted for decoding only dots first to get the correct file extension part
    df_meta_to_fetch$filenames <- gsub("=2E|%2E",".", df_meta_to_fetch$filenames)

    forbiden_chars <- "[\\/:*?\"<>|]"
    df_meta_to_fetch$filenames <- gsub(forbiden_chars, "", df_meta_to_fetch$filenames)

    # standard URLdecoding:
    for (j in seq_along(df_meta_to_fetch$filenames)) {
      df_meta_to_fetch$filenames[j] <- tryCatch({
        utils::URLdecode(df_meta_to_fetch$filenames[j])
      }, warning = function(w) {
        df_meta_to_fetch$filenames[j]
      }, error = function(e) {
        df_meta_to_fetch$filenames[j]
      })
    }

    return(df_meta_to_fetch)

  } else {

    return(NULL)

  }


  # depois que fizer esse dataframe, posso filtarr dependendo do tipo de aruqivo que o usuario quer baixar
  # se eh inline ou attachment

}
