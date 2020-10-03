#' List the server's IMAP capabilities (INTERNAL HELPER)
#' @param retries Number of attempts to connect and execute the command.
#'   Default is \code{1}.
#' @noRd
list_server_capabilities_int <- function(self, retries) {

  check_args(retries = retries) # we have to pass
  #.. the argg as arg = arg, in order to the check_argg capture the names

  # copying imapconf (in case we need to establish fresh_connect = TRUE if an error appear)
  # we dont need because we isolate the handle - it will find a new connection eventually

  # forcing retries as an integer
  retries <- as.integer(retries)

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  tryCatch({
    curl::handle_setopt(h, customrequest = "CAPABILITY")
  }, error = function(e){
    stop("The connection handle is dead. Please, configure a new IMAP connection with configure_imap().")
  })

  response <- tryCatch({
    curl::curl_fetch_memory(url, handle = h)
  }, error = function(e){
    # print(e$message)
    response_error_handling(e$message[1])
  })

  if (!is.null(response)) {

    pattern = '\\* CAPABILITY (.*?)\r\n' # works w/ gsub in every connection

    # server_capabilities <- strsplit(
    #   x = unlist(regmatches(rawToChar(response$headers),
    #                         regexec(pattern,
    #                                 rawToChar(response$headers),
    #                                 perl=TRUE)))[2],
    #   split = " ") # inconsistent

    server_capabilities <- unlist(regmatches(rawToChar(response$headers),
                                             gregexpr(pattern,
                                                      rawToChar(response$headers)
                                             )))

    server_capabilities <- strsplit(gsub('\\* CAPABILITY |\r\n', '',
                                         server_capabilities[length(server_capabilities)]
                                         ),
                                    split = " ")

    # sanitizing
    rm(h)
    rm(response)

  } else {
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

    if (!is.null(response)) {
      pattern = '\\* CAPABILITY (.*?)\r\n' # works w/ gsub in every connection

      server_capabilities <- unlist(regmatches(rawToChar(response$headers),
                                        gregexpr(pattern,
                                                 rawToChar(response$headers)
                                        )))

      server_capabilities <- strsplit(gsub('\\* CAPABILITY |\r\n', '',
                                           server_capabilities[length(server_capabilities)]
                                           ),
                                      split = " ")

      # sanitizing
      rm(h)
      rm(response)


    } else {
      stop('Request error: the server returned an error.')
    }

  }

  if (self$con_params$verbose) {
    Sys.sleep(0.01)  # wait for the end of the client-server conversation
  }
  return(unlist(server_capabilities))

}
