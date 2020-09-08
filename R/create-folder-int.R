#' @description Create a new mail folder (INTERNAL HELPER)
#' @param name A string containing the name of the new mail folder to be
#'   created.
#' @param mute A \code{logical}. Provides a confirmation message if the
#'   command is successfully executed. Default is \code{FALSE}.
#' @param retries Number of attempts to connect and execute the command.
#'   Default is \code{1}.
#' @noRd
create_folder_int <- function(self, name, mute, retries) {

  check_args(name = name, mute = mute, retries = retries) # we have to pass
  #.. the argg as arg = arg, in order to the check_argg capture the names

  # forcing retries as an integer
  retries <- as.integer(retries)

  name2 <- adjust_folder_name(name)

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  tryCatch({
    curl::handle_setopt(h, customrequest = paste0('CREATE ', name2))
  }, error = function(e){
    stop("The connection handle is dead. Please, configure a new IMAP connection with ImapConf$new().")
  })

  response <- tryCatch({
    curl::curl_fetch_memory(url, handle = h)
  }, error = function(e){
    # print(e$message)
    response_error_handling(e$message[1]) # returns NULL for operation timeout: try reconnection
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
        response_error_handling(e$message[1]) # returns NULL for operation timeout: try reconnection
      })

    }

    if (is.null(response)) {

      stop('Request error: the server returned an error.')

    } else { # v0.3.2
      if (!mute) {

        if (self$con_params$verbose) {
          Sys.sleep(0.01)  # wait for the end of the client-server conversation
        }

        cat(paste0("\n::mRpostman: folder ", '"', name, '"', " created.\n")) # v0.3.2

      }

    }

  } else {
    if (!mute) {

      if (self$con_params$verbose) {
        Sys.sleep(0.01)  # wait for the end of the client-server conversation
      }

      cat(paste0("\n::mRpostman: folder ", '"', name, '"', " created.\n")) # v0.3.2

    }
  }

  invisible(TRUE)

}
