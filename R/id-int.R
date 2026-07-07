#' Exchange client/server identification (INTERNAL HELPER)
#' @param fields A named character vector with the client id fields to send
#'   (e.g. \code{c(name = "mRpostman", version = "1.2.1")}), or \code{NULL} to
#'   send \code{ID NIL}.
#' @param retries Number of attempts to connect and execute the command.
#' @noRd
id_int <- function(self, fields, retries) {

  if (!is.null(fields)) {
    assertthat::assert_that(
      is.character(fields) && !is.null(names(fields)) && all(nzchar(names(fields))),
      msg='"fields" must be a named character vector (e.g. c(name = "mRpostman")), or NULL.')
  }

  check_args(retries = retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  if (is.null(fields)) {
    customrequest <- "ID NIL"
  } else {
    pairs <- paste0('"', names(fields), '" "', unname(fields), '"', collapse = " ")
    customrequest <- paste0("ID (", pairs, ")")
  }

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  tryCatch({
    curl::handle_setopt(h, customrequest = customrequest)
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
  final_output <- parse_id(resp_char)

  # sanitizing
  rm(h)
  rm(response)

  if (self$con_params$verbose) {
    Sys.sleep(0.01)  # wait for the end of the client-server conversation
  }
  return(final_output)

}
