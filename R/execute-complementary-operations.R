#' Execution engine for all the complementary commands
#' @param self The R6 connection object.
#' @param url A string containing the url from the \code{IMAP_conn$imapconf} object.
#' @param handle A curl handle object with the custom request already defined.
#' @param customrequest A string containing the custom request to the server that will
#'     be added to the curl handle.
#' @param retries Number of attempts to connect and execute the command. Default
#'     is \code{1}.
#'   @noRd
execute_complementary_operations <- function(self, url, handle,  customrequest,
                                             retries) {

  # previous folder selection checking
  # if (is.na(self$folder)) {
  #   stop('No folder previously selected.')
  # }
  assertthat::assert_that(
    !is.na(self$con_params$folder),
    msg='No folder previously selected.')

  tryCatch({
    curl::handle_setopt(
      handle = handle,
      customrequest = customrequest)
  }, error = function(e){
    stop("The connection handle is dead. Please, configure a new IMAP connection with ImapCon$new().")
  })

  # REQUEST
  response <- tryCatch({
    curl::curl_fetch_memory(url, handle = handle)
  }, error = function(e){
    # print(e$message)
    response_error_handling(e$message[1]) # returns NULL for operation timeout: try reconnection
  })

  if (is.null(response)) {

    # it is not necessary to select again
    count_retries = 0 #the first try was already counted
    # FORCE appending fresh_connect
    # curl::handle_setopt(handle = h, fresh_connect = TRUE)
    select_folder_int(self, name = self$con_params$folder, mute = TRUE, retries = 0) # ok! v0.0.9

    while (is.null(response) && count_retries < retries) {
      count_retries = count_retries + 1

      # reset customrequest in handle
      tryCatch({
        curl::handle_setopt(
          handle = handle,
          customrequest = customrequest)
      }, error = function(e){
        stop("The connection handle is dead. Please, configure a new IMAP connection with ImapCon$new().")
      })

      # REQUEST
      response <- tryCatch({
        curl::curl_fetch_memory(url, handle = handle)
      }, error = function(e){
        # print(e$message)
        response_error_handling(e$message[1]) # returns NULL for operation timeout: try reconnection
      })
    }

    if (is.null(response)) {
      stop('Request error: the server returned an error.')
    }

  }

  # handle sanitizing
  rm(handle)

  return(response)

}
