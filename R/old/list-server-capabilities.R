#' @title IMAP Server Capabilities v0.3.2-X
#'
#' @description Lists IMAP server's capabilities.
#'
#' @param retries Number of attempts to connect and execute the command. Default
#'     is \code{2}.
#'
#' @return A character \code{vector} containing server's IMAP capacbilities.
#'
#' @family mailbox commands
#'
#' @examples
#' \dontrun{
#'
#' # configure IMAP
#' #' library(mRpostman)
#' configure_imap(url="imaps://your.imap.server.com",
#'                username="your_username",
#'                password=rstudioapi::askForPassword()
#'                )
#'
#' # list server's capabilities
#' list_server_capabilities()
#'
#' }
#'
#' @export
#'
list_server_capabilities <- function(retries = 2) {

  assertthat::assert_that(
    is.numeric(retries),
    assertthat::validate_that(retries >= 1),
    msg='"retries" must be an integer equal or greater than 1.')

  if(retries%%1 != 0){
    warning('only the integer part of "retries" will be used.')
  }

  # copying imapconf (in case we need to establish fresh_connect = TRUE if an error appear)
  # we dont need because we isolate the handle - it will find a new connection eventually

  # forcing retries as an integer
  retries <- as.integer(retries)

  # forcing server url to the format we need -- removing unnecessary final slashe(s)
  #.. and doing URL encoding
  url <- utils::URLencode(gsub("/+$","", IMAP_conn$imapconf$url))

  # isolating the handle
  h <- IMAP_conn$imapconf$conn_handle

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
    # pattern = '.*\\* CAPABILITY (.*?)\r\n$' # works w/ gsub in every connection

    # server_capabilities <- gsub(pattern, "\\1", rawToChar(response$headers)) # diff to list_mailboxes -- parse headers
    # server_capabilities <- gsub('\r\n.*$', "", server_capabilities) # cleaning when we have the first connection
    #
    # server_capabilities <- strsplit(x = server_capabilities, split = " ")

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
    count_retries = 1 #the first try was already counted

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

  return(unlist(server_capabilities))

}
