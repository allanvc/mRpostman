#' @title Get Attachments
#'
#' @description Get attachments after fetching full messages.
#'
#' @inheritParams check_args_get_attachments
#'
#' @return A logical \code{TRUE} if the attachments extraction was successful.
#'
#' @note All attachments will be stored in a folder labelled with the message id
#'     inside the working directory. This function currently handles only attachments
#'     encoded as \code{base64} text. It tries to guess all file extensions while
#'     decoding the text, but it may not be possible to do it in some circumstances.
#'     If it happens, you can try to change the file extension directly by renaming
#'     the file.
#'
#' @note The "Content-Disposition" header specifies if the multipart electronic
#'     messages will be presented as a main document with a list of separate
#'     attachments ("Content-Disposition: attachment") or as a single document
#'     with the various parts displayed inline. The first requires positive action
#'     on the part of recipient, i.e. downloading the file, whereas inline
#'     components are displayed automatically when the message is viewed
#'     (\cite{Troost, R., Dorner, S., and K. Moore, Ed. (1997)}). You can choose
#'     to download \code{both}, or only one of the types of attachments, using the
#'     argument \code{content_disposition}.
#'
#' @references{
#'     Troost, R., Dorner, S., and K. Moore, Ed.,
#'     \emph{Communicating Presentation Information in Internet Messages:
#'     The Content-Disposition Header Field}, RFC 2183, DOI 10.17487/RFC2183,
#'     August 1997, \url{https://www.rfc-editor.org/info/rfc2183}.}
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
#' # extracting attachments
#' imapconf %>%
#'     select_mailbox(mbox = "TAM") %>%
#'     search_before(date_char = "10-may-2012", by = "UID") %$% #exposition pipe - two argg
#'     fetch_full_msgs(imapconf = imapconf, msg_id = msg_id) %>%
#'     get_attachments()
#'
#' }
#'
#' @export
#'
get_attachments <- function(msg_list, content_disposition = "both") {

  #check
  check_args_get_attachments(msg_list, content_disposition)

  # retireves only base64 encoded attachments for now

  # preparing mbox part of the directory for saving
  mbox = attr(msg_list, "mbox")
  mbox_clean = gsub("%20", "_", mbox)
  forbiden_chars <- "[\\/:*?\"<>|]"
  mbox_clean = gsub(forbiden_chars, "", mbox_clean)

  for (i in seq_along(msg_list)) {

    id = names(msg_list[i]) # doing this to conserve name attribute

    msg = msg_list[[i]]

    if (has_attachment(msg)) {

      # 1) full attachments excerpts (with attachment "headers")
      # v0.3.1 - added support to inline attachments; added content_disposition argument
      if(content_disposition == "attachment"){
        pattern = 'Content-Disposition: attachment;[\t|\r|\n|\r\n|a-zA-Z0-9 ]+(.*?)--'



      } else if(content_disposition == "inline"){
        pattern = 'Content-Disposition: inline;[\t|\r|\n|\r\n|a-zA-Z0-9 ]+(.*?)--'

      } else {
        pattern = 'Content-Disposition: attachment;[\t|\r|\n|\r\n|a-zA-Z0-9 ]+(.*?)--|Content-Disposition: inline;[\t|\r|\n|\r\n|a-zA-Z0-9 ]+(.*?)--'

      }

      # this REGEX works with IMAP and MS/Exchange protocols
      full_attachments <- unlist(regmatches(msg, gregexpr(pattern, msg)))
      # starting from full attachments to get filenames and text after

      # 2) extract only text
      # part1
      pattern = '\\r\\n[A-Za-z0-9+\\/]{76,76}.*' # attachments parts have 76 characters each line
      attachments_text <- unlist(regmatches(full_attachments,
                                            gregexpr(pattern, full_attachments)))

      attachments_text <- gsub("^\r\n","", attachments_text) # "beggining with"
      attachments_text <- gsub("[\r\n]+[-]*$","", attachments_text) # "ending with"


      # 3) extract attachment filenames:
      # v0.3.1 - simplified REGEX - we do not need to worry about getting ordinary text
      # ..we have already selected only the attachments
      pattern = 'filename=\"(.*?)\"[\r\n|;]'
      # this REGEX works with IMAP and MS/Exchange protocols
      filenames <- unlist(regmatches(full_attachments, regexec(pattern, full_attachments)))

      # v0.3.1 - to avoid errors in case NULL
      if(!is.null(filenames)){ # here we have to run the rest of the code inside the condition

        filenames <- filenames[seq(2, length(filenames), by = 2)]

        # cleaning encoding strings in filenames, e.g. "=?Windows-1252?Q?Termo_de_extra_SIAPE.?=\r\n =?Windows-1252?Q?pdf?="
        # pasting the extension to the name when it is
        filenames <- gsub("\\?=\r\n\\s*|=\\?[A-Za-z0-9-]+\\?Q\\?|\\?=$","", filenames)
        # "ending with"

        # substituting URI encoding of a dot (=2E|%2E) -- it happens with yandex mail in some cases
        # we opted for decoding only dots first to get the correct file extension part
        filenames <- gsub("=2E|%2E",".", filenames)

        # standard URLdecoding:
        for (j in seq_along(filenames)) {
          filenames[j] <- tryCatch({
            filenames[j] <- utils::URLdecode(filenames[j])
          }, warning = function(w) {
            filenames[j]
          }, error = function(e) {
            filenames[j]
          })
        }

        # removing problematic Win-*NIX-OSX characters from filenames
        # forbiden_chars <- "[\\/:*?\"<>|]"
        filenames <- gsub(forbiden_chars,"", filenames)

        # getting attachments encoding
        # pattern = '\r\nContent-Transfer-Encoding: (.*?)[\r\n|\r|\n]+'
        # this REGEX works with IMAP and MS/Exchange protocols

        # encodings <- unlist(regmatches(full_attachments, regexec(pattern, full_attachments)))
        # encodings <- encodings[seq(2, length(encodings), by = 2)]
        # note to self: encodings sometimes comes before "Content-Disposition: attachment;"
        # bring this feature only on future improvements

        # if (any(!grepl(pattern = "base64", x = encodings))) {
        #   warning("There are one or more non-base64 encoded attachments that will not be decoded. Use list_attachments() to identify them.")
        # }

        # preparing the directory for saving

        complete_path <- paste0("./", mbox_clean, "/", id)
        dir.create(path = complete_path, showWarnings = FALSE, recursive = TRUE)

        # looping on attachments
        for (i in seq_along(filenames)) {

          # # base64 encoding
          # if (encodings[i] == "base64") {

          # saving attachments
          # thank's to:
          # https://stackoverflow.com/questions/36708191/convert-base64-to-png-jpeg-file-in-r
          # writing binary file
          temp_bin_name <- paste0(sample(letters, 4), sample(0:9, 4), collapse="")
          conn <- file(paste0(complete_path, "/", temp_bin_name, ".bin"),"wb")
          writeBin(attachments_text[i], conn)
          close(conn)
          # decoding from BIN to the appropriate file extension
          inconn <- file(paste0(complete_path, "/", temp_bin_name, ".bin"),"rb")
          outconn <- file(paste0(complete_path, "/", filenames[i]),"wb")

          # base64 text decoding
          tryCatch({
            base64enc::base64decode(what=inconn, output=outconn)
          }, error = function(e) {
            warning(paste0("Base64 text decoding failed for", filenames[i]))
          })

          close(inconn)
          close(outconn)

          unlink(paste0(complete_path, "/", temp_bin_name, ".bin")) # deleting binary file
          # From unlink() help: Not deleting a non-existent file is not a failure
          # we don't need a tryCatch()

        }


      } else {

        message('No attachments with the specified "content_disposition" were found!')

      }

    } # end has_attachment()

  }

  invisible(TRUE)
}
