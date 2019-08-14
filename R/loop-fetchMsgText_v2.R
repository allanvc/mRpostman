#' @title Metadata Fetch Loop
#'
#' @inherit loop_fetchFullMsg description return
#'
#' @param new_imapconf An object of class \code{imapconf} generated with
#'     \link{configureIMAP} and with a \code{mbox} item added after a
#'     mailbox selection with \link{selectMailbox}.
#' @inheritParams check_args_fetchMsgText
#' @param handle A curl handle object.
#'
#' @return A \code{list} or/and text files containing the fetch results.
#'
#' @family fetch helpers
#' @family loop
#'
#' @keywords internal
#'
loop_fetchMsgText <- function(new_imapconf, msg_id, by, peek,
                              partial, write_to_disk, keep_in_mem,
                              retries, handle){

  h = handle


  # preparation

  # peek
  if(isTRUE(peek)){
    body_string = " BODY.PEEK[TEXT]"
  } else {
    body_string = " BODY[TEXT]"
  }

  # partial
  if(!is.null(partial)){
    partial_string = paste0("<", partial, ">")
  } else {
    partial_string = NULL
  }

  # by
  if (by == "UID") {
    by_string = "UID "
  } else {
    by_string = NULL
  }


  # fetching
  msg_list <- list()
  idx = 0

  for(id in msg_id){
    idx = idx+1

    curl::handle_setopt(
      handle = h,
      customrequest = paste0(by_string, "FETCH ", id, body_string, partial_string))

    response <- tryCatch({

      curl::curl_fetch_memory(url = new_imapconf$url, handle = h)

    }, error = function(e){
      return(NULL)

    })

    if(!is.null(response)){
      msg_list[[idx]] <- decode_base64_text_if_TRUE(
        clean_fetch_results(
          rawToChar(response$headers)
        )
      )

      rm(response)

      if(by == "UID"){
        names(msg_list)[idx] <- paste0("textUID", id)

      } else{
        names(msg_list)[idx] <- paste0("textSEQID", id)

      }

      if(isTRUE(write_to_disk)){

        mbox_clean = gsub("%20", "_", new_imapconf$mbox)

        forbiden_chars <- "[\\~#%&*{}/:<>?|\"-]"
        mbox_clean = gsub(forbiden_chars, "", mbox_clean)

        complete_path <- paste0("./", mbox_clean)
        dir.create(path = complete_path, showWarnings = FALSE, recursive = TRUE)

        write(unlist(msg_list[[idx]]), paste0(complete_path, "/",
                                              names(msg_list)[idx], ".txt"))

        if(isFALSE(keep_in_mem)){
          msg_list[[id]] <- NA
        }

      }

    } else {
      count_retries = 1 #the first try was already counted
      # FORCE appending fresh_connect
      curl::handle_setopt(handle = h, fresh_connect = TRUE)

      while(is.null(response) && count_retries < retries){
        count_retries = count_retries+1

        response <- tryCatch({

          curl::curl_fetch_memory(url = new_imapconf$url, handle = h)

        }, error = function(e){
          return(NULL)

        })

        if(!is.null(response)){
          msg_list[[idx]] <- decode_base64_text_if_TRUE(
            clean_fetch_results(
              rawToChar(response$headers)
              )
            )

          rm(response)

          if(by == "UID"){
            names(msg_list)[idx] <- paste0("textUID", id)

          } else{
            names(msg_list)[idx] <- paste0("textSEQID", id)

          }

          if(isTRUE(write_to_disk)){

            mbox_clean = gsub("%20", "_", new_imapconf$mbox)

            forbiden_chars <- "[\\~#%&*{}/:<>?|\"-]"
            mbox_clean = gsub(forbiden_chars, "", mbox_clean)

            complete_path <- paste0("./", mbox_clean)
            dir.create(path = complete_path, showWarnings = FALSE, recursive = TRUE)

            write(unlist(msg_list[[idx]]), paste0(complete_path, "/",
                                                  names(msg_list)[idx], ".txt"))

            if(isFALSE(keep_in_mem)){
              msg_list[[id]] <- NA
            }

          }
        } else{
          stop('An error ocurred while connecting. Please check the following and/or try again:\n
         - your internet connection status;\n
         - if imapconf options are valid;\n
         - the name of the Mailbox (argument "mbox");\n
         - if "msg_id" does not have any number greater than "EXISTS" -- see examineMailbox().'

          )
        }
      } #while
    } #else-response
  } #for

  if(isFALSE(keep_in_mem)){
    rm(msg_list)
    return(TRUE)

  } else{
    return(msg_list)
  }

}
