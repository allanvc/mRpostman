#' Get the quota root(s) and quota of a mail folder (INTERNAL HELPER)
#' @param name A string with the mail folder name. If \code{NULL}, uses the
#'   previously selected folder.
#' @param retries Number of attempts to connect and execute the command.
#' @noRd
get_quota_root_int <- function(self, name, retries) {

  if (!is.null(name)) {
    assertthat::assert_that(
      is.character(name),
      msg='"name" must be of type character or NULL.')
  } else {
    assertthat::assert_that(
      !is.na(self$con_params$folder),
      msg='No folder previously selected.')
  }

  check_args(retries = retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  folder <- if (is.null(name)) {
    adjust_folder_name(self$con_params$folder)
  } else {
    adjust_folder_name(name)
  }

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  tryCatch({
    curl::handle_setopt(h, customrequest = paste0("GETQUOTAROOT ", folder))
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

  # the untagged * QUOTA line may arrive via headers or content
  resp_char <- paste(rawToChar(response$headers),
                     rawToChar(response$content), sep = "\r\n")
  final_output <- parse_quota(resp_char)

  # sanitizing
  rm(h)
  rm(response)

  if (self$con_params$verbose) {
    Sys.sleep(0.01)  # wait for the end of the client-server conversation
  }
  return(final_output)

}
