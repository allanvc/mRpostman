#' Select a mail folder (INTERNAL HELPER).
#' @param name A \code{character} string containing the name of an existing mail folder on the
#'   user's mailbox.
#' @param mute A \code{logical}. Provides a confirmation message if the
#'   command is successfully executed. Default is \code{FALSE}.
#' @param retries Number of attempts to connect and execute the command. Default
#'   is \code{1}.
#' @noRd
select_folder_int <- function(self, name, mute, retries) {

  check_args(name = name, mute = mute, retries = retries) # we have to pass
  #.. the argg as arg = arg, in order to the check_argg capture the names

  # forcing retries as an integer
  retries <- as.integer(retries)

  folder <- adjust_folder_name(name)
  # folder = name

  # self$imapconf$url <- utils::URLencode(gsub("/+$", "", self$url))
  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle
  # h <- curl::new_handle()
  # do.call(curl::handle_setopt, c(h, con$self)) # da erro aqui. eh melhor passar o handle de self igual ele cria o
  # self$auth

  tryCatch({
    curl::handle_setopt(h, customrequest = paste0('SELECT ', folder))
  }, error = function(e){
    stop("The connection handle is dead. Please, configure a new IMAP connection with configure_imap().")
  })

  response <- tryCatch({
    curl::curl_fetch_memory(url, handle = h)
  }, error = function(e){
    # print(e$message)
    response_error_handling(e$message[1])
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
        response_error_handling(e$message[1])
      })

    }

    if (is.null(response)) {

      stop('Request error: the server returned an error.')

    } else { # v0.3.2
      if (!mute) {
        if (self$con_params$verbose) {
          Sys.sleep(0.01)  # wait for the end of the client-server conversation
        }
        cat(paste0("\n::mRpostman: ", '"', name, '"', " selected.\n")) # v0.3.2
        # using the folder name without any transformation
      }

    }

  } else {
    if (!mute) {
      if (self$con_params$verbose) {
        Sys.sleep(0.01)
      }
      cat(paste0("\n::mRpostman: ", '"', name, '"', " selected.\n")) # v0.3.2
      # using the folder name without any transformation
    }
  }


  invisible(name)
  # invisible(0L)

}
