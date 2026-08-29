#' @description Create a new mail folder (INTERNAL HELPER)
#' @param name A string containing the name of the new mail folder to be
#'   created.
#' @param mute A \code{logical}. Provides a confirmation message if the
#'   command is successfully executed. Default is \code{FALSE}.
#' @param retries Number of attempts to connect and execute the command.
#'   Default is \code{1}.
#' @noRd
create_folder_int <- function(self, name, mute, retries, special_use = NULL) {

  check_args(name = name, mute = mute, retries = retries) # we have to pass
  #.. the argg as arg = arg, in order to the check_argg capture the names

  # CREATE-SPECIAL-USE (RFC 6154, section 5.3): "CREATE name (USE (\\Attr))"
  use_string <- ""
  if (!is.null(special_use)) {
    valid_use <- c("\\All", "\\Archive", "\\Drafts", "\\Flagged", "\\Junk",
                   "\\Sent", "\\Trash")
    assertthat::assert_that(
      is.character(special_use), all(special_use %in% valid_use),
      msg=paste0('"special_use" must be NULL or a subset of: ',
                 paste(valid_use, collapse = ", "), '.'))
    assert_capability(self, "CREATE-SPECIAL-USE", command = "create_folder(special_use = ...)",
                      rfc = "RFC 6154", retries = retries)
    use_string <- paste0(" (USE (", paste(special_use, collapse = " "), "))")
  }

  # forcing retries as an integer
  retries <- as.integer(retries)

  name2 <- adjust_folder_name(name)

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  tryCatch({
    curl::handle_setopt(h, customrequest = paste0('CREATE ', name2, use_string))
  }, error = function(e){
    stop("The connection handle is dead. Please, configure a new IMAP connection with ImapCon$new().")
  })

  response <- tryCatch({
    curl::curl_fetch_memory(url, handle = h)
  }, error = function(e){
    # print(e$message)
    response_error_handling(e$message[1], self) # returns NULL for operation timeout: try reconnection
  })

  if(is.null(response)){

    count_retries = 0 #the first try doesnt count

    # FORCE appending fresh_connect
    # curl::handle_setopt(handle = h, fresh_connect = TRUE) # parece que nao precisa, mas vamos deixar

    while (is.null(response) && count_retries < retries) {
      count_retries = count_retries + 1

      response <- tryCatch({
        curl::curl_fetch_memory(url, handle = h)
      }, error = function(e){
        # print(e$message)
        response_error_handling(e$message[1], self) # returns NULL for operation timeout: try reconnection
      })

    }

    if (is.null(response)) {

      stop('Request error: the server returned an error.')

    } else { # v0.3.2
      if (!mute) {


        cat(paste0("\n::mRpostman: folder ", '"', name, '"', " created.\n")) # v0.3.2

      }

    }

  } else {
    if (!mute) {


      cat(paste0("\n::mRpostman: folder ", '"', name, '"', " created.\n")) # v0.3.2

    }
  }

  invisible(TRUE)

}
