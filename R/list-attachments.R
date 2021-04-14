#' List attachments and content-disposition types
#' @param msg_list A \code{list} containing the messages (body or text) fetched
#'   from the server.
#' @note Please, note that this is an independent function and not an R6 method
#'   that depends on the connection object. Therefore, it should be called alone
#'   without the ImapCon object.
#' @return A \code{list} of \code{data.frames} containing the filenames and its
#'   \code{Content-Disposition} types for each fetched message.
#' @family attachments
#' @examples
#' \dontrun{
#' con$select_folder(name = "INBOX")
#' # do a search followed by a fetch operation, then extract the attachments' list
#' out <- con$search_string(expr = "@k-state.edu", where = "FROM") %>%
#'   con$fetch_body()
#' att_list <- list_attachments(msg_list = out)
#'
#' # or
#' att_list <- con$search_string(expr = "@k-state.edu", where = "FROM") %>%
#'   con$fetch_body() %>%
#'   list_attachments()
#' }
#' @export
#'
list_attachments <- function(msg_list) {

  #check
  check_args(msg_list = msg_list)

  attachments_list <- list()

  # retireves only base64 encoded attachments for now

  for (i in seq_along(msg_list)) {

    # i = 1

    id = names(msg_list[i]) # doing this to conserve name attribute

    msg = msg_list[[i]]

    if (has_attachment(msg, call_from = "list_attachments")) {

      # A) Content_Disposition: attachment
      # 1) full attachments excerpts (with attachment "headers")
      # v0.3.1 - changed REGEX for getting full attachments
      pattern = 'Content-Disposition: attachment;[\t|\r|\n|\r\n|a-zA-Z0-9 ]+(.*?)--|Content-Disposition: inline;[\t|\r|\n|\r\n|a-zA-Z0-9 ]+(.*?)--'
      # this REGEX works with IMAP and MS/Exchange protocols
      full_attachments <- unlist(regmatches(msg, gregexpr(pattern, msg)))
      # starting from full attachments to get filenames and text after

      # 2) extract attachment filenames:
      # pattern = '\r\nContent-Disposition: attachment;[\r\n\t]|[\r\n]*filename=\"(.*?)\"[\r\n|;]'
      # v0.3.1 - simplified REGEX - we do not need to worry about getting ordinary text
      # ..we have already selected only the attachments
      pattern = 'filename=\"(.*?)\"[\r\n|;]'
      # this REGEX works with IMAP and MS/Exchange protocols

      filenames <- unlist(regmatches(full_attachments, regexec(pattern, full_attachments)))

      # v0.3.1 - to avoid errors in case NULL
      if(!is.null(filenames)){
        filenames <- filenames[seq(2, length(filenames), by = 2)]
      }


      # cleaning encoding strings in filenames, e.g. "=?Windows-1252?Q?Termo_de_extra_SIAPE.?=\r\n =?Windows-1252?Q?pdf?="
      # pasting the extension to the name when it is
      # v 0.9.1
      # rfc2047 mime header decoding
      filenames <- decode_mime_header(string = filenames)
      # filenames <- gsub("\\?=\r\n\\s*|=\\?[A-Za-z0-9-]+\\?Q\\?|\\?=$","", filenames)
      # "ending with"

      # substituting URI encoding of a dot (=2E|%2E) -- it happens with yandex mail in some cases
      # we opted for decoding only dots first to get the correct file extension part
      # filenames <- gsub("=2E|%2E",".", filenames)

      # standard URLdecoding:
      # for (j in seq_along(filenames)) {
      #   filenames[j] <- tryCatch({
      #     filenames[j] <- utils::URLdecode(filenames[j])
      #   }, warning = function(w) {
      #     filenames[j]
      #   }, error = function(e) {
      #     filenames[j]
      #   })
      # }

      # v0.3.1 - obtaining Content-Disposition types
      pattern = 'Content-Disposition: (inline|attachment);'
      cont_disp_types <- unlist(regmatches(full_attachments, regexec(pattern, full_attachments)))

      if(!is.null(cont_disp_types)){
        cont_disp_types <- cont_disp_types[seq(2, length(cont_disp_types), by = 2)]
      }

      # 3) getting attachments encoding
      # pattern = '\r\nContent-Transfer-Encoding: (.*?)[\r\n|\r|\n]+'
      # this REGEX works with IMAP and MS/Exchange protocols

      # encodings <- unlist(regmatches(full_attachments, regexec(pattern, full_attachments)))
      # encodings <- encodings[seq(2, length(encodings), by = 2)]
      # note to self: encodings sometimes comes before "Content-Disposition: attachment;"
      # bring this feature only on future improvements

      # if (any(!grepl(pattern = "base64", x = encodings))) {
      #   warning("There are one or more non-base64 encoded attachments that will not be decoded. Use list_attachments() to identify them.")
      # }

      # note to future me: I had the idea of extracting filetypes, but sometimes
      #"Content-Type" is before "Content-Disposition" and we lose it after getting
      #full_attachments object

      out_df <- data.frame("filename" = filenames,
                           "content_disposition" = cont_disp_types,
                           # "encoding" = encodings,
                           stringsAsFactors = FALSE)

      # binding regular attachments and inline attachments
      out <- list(out_df)

      names(out) <- id

      attachments_list <- c(attachments_list, out)


    } else { # when has_attachments returns FALSE

      out <- NA
      names(out) <- id
      attachments_list <- c(attachments_list, out)

    }


  }

  return(attachments_list)

}
