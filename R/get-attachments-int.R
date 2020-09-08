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
#' @noRd
get_attachments_int <- function(self, msg_list, content_disposition, override,
                                mute) {

  # previous folder selection checking
  if (is.na(self$con_params$folder)) {
    stop('No folder previously selected.')
  }

  #check
  check_args(msg_list = msg_list, content_disposition = content_disposition,
             override = override, mute = mute)

  # retireves only base64 encoded attachments for now

  # preparing mbox part of the directory for saving
  # folder <- adjust_folder_name(self$folder)

  # mbox = attr(msg_list, "mbox")
  folder_clean = gsub("%20", "_", self$con_params$folder)
  forbiden_chars <- "[\\/:*?\"<>|]"
  folder_clean = gsub(forbiden_chars, "", folder_clean)

  for (i in seq_along(msg_list)) {
    # i = 1

    id = names(msg_list[i]) # doing this to conserve name attribute
    id = unlist(regmatches(id, regexec("UID\\d+|\\d+", id)))

    msg = msg_list[[i]]

    if (has_attachment(msg, call_from = "get_attachments")) {

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
      # substr(full_attachments[1], 1, 1000)
      # starting from full attachments to get filenames and text after

      # 2) extract only text

      # part1
      # pattern = '\\r\\n[A-Za-z0-9+\\/]{0,76}.*' # attachments parts can have a maximum 76 column (characters) (MIME standards)
      # # pattern = '\\r\\n[A-Za-z0-9+\\/]+.*' # v0.0.9.0 the case when the attachments do not have 76 columns of characters forced us to implement some changes
      #
      # attachments_text <- unlist(regmatches(full_attachments,
      #                                       gregexpr(pattern, full_attachments)))


      attachments_text <- sub('.*Content-', '', full_attachments, ignore.case = TRUE) # sub extracts only the first match
      # we extract from the last match onwards

      pattern2 = "\\r\\n[^ ]+\\r\\n" # lines that do not contain space

      attachments_text <- unlist(regmatches(attachments_text,
                                            regexec(pattern2, attachments_text, perl=TRUE)))

      # it has to be in this order

      # substr(attachments_text[1], 1, 1000)
      # deu erro em um do yahoo
      # 1) podemos ou pegar tudo que vier depois de \\r\\n\\r\\n pq vem sempre pulado linha
      # 2) ou entao tudo que tiver depois da ultima aparicao da palvra content, depois
      # descontar tudo que aparece depois do primeiro \\r\\n, depois limpar algum que sobrar
      # 3) tudo que tiver depois da ultima aparicao da palvra content, depois
      # descartar tudo que aparece entre content e \\r\\n que contenha primeiro espaco (copiar estrategioa anterior),
      # depois limpar algum que sobrar

      # pattern2 = "\\r\\n\\r\\n.*"
      # # pattern2 = "\\r\\n\\r\\n(.*)"
      # pattern2 = "\r\n+[^\r\n]*\r\n*(?!.*Content).*"
      #
      # pattern2 = "[\\r\\n]+(?!.*Content).*"
      #
      # pattern2 = ".*[^(Content)]+$"


      # attachments_text <- gsub("^\r\n","", attachments_text) # "beggining with" #
      attachments_text <- gsub("^[\r\n]+","", attachments_text) # "beggining with" #V0.9.0.0

      # attachments_text <- gsub("^Content-Transfer-Encoding: base64\r\n\r\n","", attachments_text,
      #                          ignore.case = TRUE) # "beggining with"

      attachments_text <- gsub("[\r\n]+[-]*$","", attachments_text) # "ending with"


      # 3) extract attachment filenames:
      # v0.3.1 - simplified REGEX - we do not need to worry about getting ordinary text
      # ..we have already selected only the attachments
      pattern = 'filename=\"(.*?)\"[\r\n|;]'
      # this REGEX works with IMAP and MS/Exchange protocols
      filenames <- unlist(regmatches(full_attachments, regexec(pattern, full_attachments)))

      # sanitizing
      rm(full_attachments)

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
        adjusted_filenames <- adjust_repeated_filenames(filenames)

        # url <- "imaps://outlook.office365.com/"
        url_folder <- unlist(regmatches(self$con_params$url, gregexpr("://(.*?)(/|.)$", self$con_params$url)))
        url_folder = gsub(forbiden_chars, "", url_folder)

        complete_path <- paste0("./", url_folder, "/", folder_clean, "/", id)
        # complete_path <- paste0("./", folder_clean, "/", id)
        dir.create(path = complete_path, showWarnings = FALSE, recursive = TRUE)

        # looping on attachments
        for (i in seq_along(adjusted_filenames)) {

          # # base64 encoding
          # if (encodings[i] == "base64") {
          if (isTRUE(override)) {
            complete_path_with_filename <- paste0(complete_path, "/", adjusted_filenames[i])
          } else {
            # AJUSTAR AQUI:
            # * Connection #4 to host outlook.office365.com left intact
            # Error in file(complete_path_with_filename, "wb") :
            #   cannot open the connection
            # In addition: Warning message:
            #   In file(complete_path_with_filename, "wb") :
            #   cannot open file '(1)./outlook.office365.com/INBOX/UID59788/image001.png': No such file or directory
            # adjusted_filenames <- serialize(adjusted_filenames)
            # complete_path_with_filename <- serialize_filename(
            #   sufix = paste0(complete_path, "/", adjusted_filenames[i]))
            complete_path_with_filename <- serialize_filename(
              sufix = adjusted_filenames[i], complete_path = complete_path)
          }
          # complete_path_with_filename <- serialize_filename(
          #   prefix = paste0(complete_path, "/", filenames[i]))

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
          outconn <- file(complete_path_with_filename,"wb")

          # base64 text decoding
          tryCatch({
            base64enc::base64decode(what=inconn, output=outconn)
          }, error = function(e) {
            warning(paste0("Base64 text decoding failed for", adjusted_filenames[i]))
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

  if (!mute) {
    cat(paste0("\n::mRpostman: attachment(s) extraction is complete."))
  }

  return(TRUE)
}
