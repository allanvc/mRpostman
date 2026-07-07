#' Get the quota of a quota root (INTERNAL HELPER)
#' @param quota_root A string with the quota root name (often \code{""} for the
#'   default root).
#' @param retries Number of attempts to connect and execute the command.
#' @noRd
get_quota_int <- function(self, quota_root, retries) {

  assertthat::assert_that(
    is.character(quota_root),
    msg='"quota_root" must be of type character.')

  check_args(retries = retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  root <- adjust_folder_name(quota_root) # quotes it ("" -> "\"\"")

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  tryCatch({
    curl::handle_setopt(h, customrequest = paste0("GETQUOTA ", root))
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
