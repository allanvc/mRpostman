#' @description List mail folders in a mailbox (INTERNAL HELPER)
#' @param retries Number of attempts to connect and execute the command.
#' @noRd
list_mail_folders_int <- function(self, retries, detailed = FALSE) {

  check_args(retries = retries) # we have to pass
  #.. the argg as arg = arg, in order to the check_argg capture the names

  assertthat::assert_that(is.logical(detailed), length(detailed) == 1,
                          msg='"detailed" must be a logical.')

  if (isTRUE(detailed)) {
    # LIST-EXTENDED (RFC 5258): one LIST with the folder attributes returned
    assert_capability(self, "LIST-EXTENDED", command = "list_mail_folders(detailed = TRUE)",
                      rfc = "RFC 5258", retries = retries)
    resp_char <- execute_simple_command(
      self, 'LIST "" "*" RETURN (CHILDREN SUBSCRIBED SPECIAL-USE)', retries)
    return(parse_list_extended(resp_char))
  }

  # forcing retries as an integer
  retries <- as.integer(retries)

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  tryCatch({
    curl::handle_setopt(h, customrequest = 'LIST "" *')
  }, error = function(e){
    stop("The connection handle is dead. Please, configure a new IMAP connection with configure_imap().")
  })

  response <- tryCatch({
    curl::curl_fetch_memory(url, handle = h)
  }, error = function(e){
    # print(e$message)
    response_error_handling(e$message[1])
  })

  if (is.null(response)) {
    count_retries = 0 #the first try doesnt count

    # FORCE appending fresh_connect
    # curl::handle_setopt(handle = h, fresh_connect = TRUE)

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
    }
  }

  # v1.1.7 - parsing extracted to the shared parse_folder_list() helper
  final_output <- parse_folder_list(rawToChar(response$content), command = "LIST")

  # sanitizing
  rm(h)
  rm(response)

  if (self$con_params$verbose) {
    Sys.sleep(0.01)  # wait for the end of the client-server conversation
  }
  return(final_output)

}
