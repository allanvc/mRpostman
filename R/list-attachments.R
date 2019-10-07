#' @title List Attachments
#'
#' @description List attachments filenames after fetching full messages.
#'
#' @inheritParams check_args_get_attachment
#'
#' @return A \code{list} of \code{data.frames} containing filenames and encodings
#'     for each fetched message.
#'
#' @family attachments
#'
#' @examples
#'
#' \dontrun{
#'
#' # configure IMAP
#' library(mRpostman)
#' imapconf <- configure_imap(url="imaps://your.imap.server.com",
#'                            username="your_username",
#'                            password=rstudioapi::askForPassword()
#'                           )
#'
#' # listing attachments
#' attachments <- imapconf %>%
#'     select_mailbox(mbox = "TAM") %>%
#'     search_before(date_char = "10-may-2012", by = "UID") %$% #exposition pipe - two argg
#'     fetch_full_msg(imapconf = imapconf, msg_id = msg_id) %>%
#'     list_attachments()
#'
#' }
#'
#' @export
#'
list_attachments <- function(msg_list) {

  #check
  check_args_get_attachment(msg_list)

  attachment_list <- list()

  # retireves only base64 encoded attachments for now

  for (i in seq_along(msg_list)) {

    id = names(msg_list[i]) # doing this to conserve name attribute

    msg = msg_list[[i]]

    if (has_attachment(msg)) {

      # 1) full attachments excerpts (with attachment "headers")
      pattern = 'Content-Disposition: attachment;[\t|\r|\n|\r\n|a-zA-Z0-9 ]+(.*?)--'
      # this REGEX works with IMAP and MS/Exchange protocols
      full_attachments <- unlist(regmatches(msg, gregexpr(pattern, msg)))
      # starting from full attachments to get filenames and text after

      # 2) extract attachment filenames:
      pattern = '\r\nContent-Disposition: attachment;[\r\n\t]|[\r\n]*filename=\"(.*?)\"[\r\n|;]'
      # this REGEX works with IMAP and MS/Exchange protocols

      filenames <- unlist(regmatches(full_attachments, regexec(pattern, full_attachments)))
      filenames <- filenames[seq(2, length(filenames), by = 2)]

      # cleaning encoding strings in filenames, e.g. "=?Windows-1252?Q?Termo_de_extra_SIAPE.?=\r\n =?Windows-1252?Q?pdf?="
      # pasting the extension to the name when it is
      filenames <- gsub("\\?=\r\n\\s*|=\\?[A-Za-z0-9-]+\\?Q\\?|\\?=$","", filenames)
      # "ending with"

      # substituting URI encoding of a dot (=2E|%2E) -- it happens with yandex mail in some cases
      # we opted for not decoding all characters, only dots
      filenames <- gsub("=2E",".", filenames)

      # standard URLdecoding:
      filenames <- utils::URLdecode(filenames)

      # 3) getting attachments encoding
      pattern = '\r\nContent-Transfer-Encoding: (.*?)[\r\n|\r|\n]+'
      # this REGEX works with IMAP and MS/Exchange protocols

      encodings <- unlist(regmatches(full_attachments, regexec(pattern, full_attachments)))
      encodings <- encodings[seq(2, length(encodings), by = 2)]


      out <- list(data.frame("filename" = filenames,
                             "encoding" = encodings,
                             stringsAsFactors = FALSE))

      names(out) <- id

      attachment_list <- c(attachment_list, out)



    }


  }

  return(attachment_list)

}
