#' Execution engine loop for all the fetch commands
#' @param self The R6 connection object.
#' @param id A message id obtained inside the main loop in \code{fetch_attachments_int}.
#' @param id_folder The name of the folder containing the message id.
#' @param url_folder The name of the folder containing the message url of the IMAP server.
#' @param content_disposition A \code{string} indicating which type of
#'   "Content-Disposition" attachments should be retrieved. The options are
#'   \code{both}, \code{attachment}, and \code{inline}. Default is
#'   \code{"both"}, which retrieves regular attachments ("Content-Disposition:
#'   attachment") and  inline attachments ("Content-Disposition: inline").
#' @param override A \code{logical}. If \code{TRUE}, overrides existent files
#'   containing the same name in the local directory. Default is \code{FALSE}.
#' @param df_meta_to_fetch A \code{data frame} returned by
#'   \code{extract_MIME_level_and_filename()} containing the filenames, the
#'   MIME level in which each attachment is, and the content-disposition of the
#'   file.
#' @param fetch_request A string containing the fetch request to the server that
#'   will be added to the curl handle.
#' @param folder_clean A \code{character} string containing the cleaned folder
#'   name, which will be uses to create a local folder.
#' @param retries Number of attempts to connect and execute the command. Default
#'   is \code{1}.
#'   @noRd
execute_attachment_fetch <- function(self, id, id_folder, df_meta_to_fetch, fetch_request,
                                     folder_clean, url_folder, content_disposition,
                                     override, retries) {


  url <- self$con_params$url
  # url <- con$url

  h <- self$con_handle
  # h <- con$con_handle

  # fetching
  # msg_list <- list()
  # idx = 0

  # finding and removing any repeated MIME.level
  # this situation may happen for inline attachments. Because in this case the data
  # will come with the cntent_disposition: inline plus the filename and we will call
  # get_attachments, we do not need to worry about the name as it will be identified
  # by get_attachments after fetching
  # and this willl prevent us from doing a second fetch of the same part

  df_meta_to_fetch <- df_meta_to_fetch[!duplicated(df_meta_to_fetch$MIME_level), ]

  df_meta_to_fetch$adjusted_filenames <- adjust_repeated_filenames(df_meta_to_fetch$filenames)

  # loop exec
  for (i in 1:nrow(df_meta_to_fetch)) {
    # print(i)
    # i = 1
    # idx = idx + 1

    adjusted_fetch_request <- gsub(pattern = "#", replacement = id, x = fetch_request)

    adjusted_fetch_request <- gsub(pattern = "level.MIME",
                                   replacement = df_meta_to_fetch$MIME_level[i],
                                   x = adjusted_fetch_request)

    tryCatch({
      curl::handle_setopt(
        handle = h,
        customrequest = adjusted_fetch_request)
    }, error = function(e){
      stop("The connection handle is dead. Please, configure a new IMAP connection with configure_imap().")
    })

    # REQUEST
    response <- tryCatch({
      curl::curl_fetch_memory(url, handle = h)
    }, error = function(e){
      # print(e$message)
      response_error_handling(e$message[1]) # returns NULL for operation timeout: try reconnection

    })

    # print(exists("response")); print(exists("response")); print(exists("response"))

    if (!is.null(response)) {

      attachment <- clean_fetch_results(
        rawToChar(response$headers))


    } else {
      count_retries = 0 #the first try was already counted
      # FORCE appending fresh_connect
      # curl::handle_setopt(handle = h, fresh_connect = TRUE)
      select_folder_int(self, name = self$con_params$folder, mute = TRUE, retries = 0) # ok! v0.0.9
      # select folder will automatically adjust the folde rname inside

      while (is.null(response) && count_retries < retries) {
        count_retries = count_retries + 1

        # reset customrequest in handle
        tryCatch({
          curl::handle_setopt(
            handle = h,
            customrequest = adjusted_fetch_request) #bug: response was NULL when recovering from a fetch timeout error
        }, error = function(e){
          stop("The connection handle is dead. Please, configure a new IMAP connection with ImapCon$new().")
        })

        # REQUEST
        response <- tryCatch({
          curl::curl_fetch_memory(url, handle = h)
        }, error = function(e){
          # print(e$message)
          response_error_handling(e$message[1]) # returns NULL for operation timeout: try reconnection

        })

        if (!is.null(response)) {

          attachment <- clean_fetch_results(
            rawToChar(response$headers))

        } else {
          stop('Fetch error: the server returned an error. Try to increase "timeout_ms".')

        }
      } #while
    } #else-response


    # saving the file:
    # preparing the directory for saving

    # here we test if it is the case of:
    # 1) the fetch has an inline attachment with the text part, and;
    # 2) the fetch has more than one attachment
    # OBS: has_attachment() is used as a tweak because it is able to recognize if the attachment is not "pure",
    #  like it had came with a boundary with some metadata of that part
    if (has_attachment(attachment, call_from = "fetch_attachments")) {
      msg_list <- list(attachment)
      rm(attachment)
      names(msg_list) <- id_folder
      self$get_attachments(msg_list = msg_list,
                           content_disposition = content_disposition,
                           override = override, mute = TRUE)
      # we still have to inform content_disposition in case there is two different type attachments in the same part
      #.. and one is an unwanted type

    } else {

      forbiden_chars <- "[\\/:*?\"<>|]"
      # url <- "imaps://outlook.office365.com/"
      url_folder <- regmatches(url, gregexpr("://(.*?)(/|.)$", url))
      url_folder = gsub(forbiden_chars, "", url_folder)

      complete_path <- paste0("./", url_folder, "/", folder_clean, "/", id_folder)
      # complete_path <- paste0("./", folder_clean, "/", id)
      dir.create(path = complete_path, showWarnings = FALSE, recursive = TRUE)

      if (isTRUE(override)) {
        complete_path_with_filename <- paste0(complete_path, "/",
                                              df_meta_to_fetch$adjusted_filenames[i])
      } else {
        # complete_path_with_filename <- serialize_filename(
        #   sufix = paste0(complete_path, "/", df_meta_to_fetch$adjusted_filenames[i]))
        complete_path_with_filename <- serialize_filename(
          sufix = df_meta_to_fetch$adjusted_filenames[i], complete_path = complete_path)
      }

      # looping on attachments
      # for (i in seq_along(df_meta_to_fetch$filename)) {

      # # base64 encoding
      # if (encodings[i] == "base64") {

      # saving attachments
      # thank's to:
      # https://stackoverflow.com/questions/36708191/convert-base64-to-png-jpeg-file-in-r
      # writing binary file
      temp_bin_name <- paste0(sample(letters, 4), sample(0:9, 4), collapse="")
      conn <- file(paste0(complete_path, "/", temp_bin_name, ".bin"),"wb")
      writeBin(attachment, conn)
      close(conn)
      # decoding from BIN to the appropriate file extension
      inconn <- file(paste0(complete_path, "/", temp_bin_name, ".bin"),"rb")
      outconn <- file(complete_path_with_filename,"wb")

      # base64 text decoding
      tryCatch({
        base64enc::base64decode(what=inconn, output=outconn)
      }, error = function(e) {
        warning(paste0("Base64 text decoding failed for", df_meta_to_fetch$filenames[i]))
      })

      close(inconn)
      close(outconn)

      unlink(paste0(complete_path, "/", temp_bin_name, ".bin")) # deleting binary file
      # From unlink() help: Not deleting a non-existent file is not a failure
      # we don't need a tryCatch()

      # }

    }




  } #for

  return(TRUE)

}
