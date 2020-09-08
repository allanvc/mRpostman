#' @title Get Maximum Message ID in a Mailbox
#'
#' @description Get the greater (maximum) message id in a Mailbox, considering
#'     messages with a specific flag.
#'
#' @inheritParams check_args_get_max_min_id
#'
#' @return A numeric \code{vector} of length \code{1} indicating the minimum id.
#'
#' @note \code{\link{get_max_id}} uses \code{ESEARCH} extension. Check if your server
#'     supports \code{ESEARCH} capability with \code{\link{list_server_capabilities}}.
#'
#' @family miscellaneous
#' @family execution
#'
#' @examples
#'
#' \dontrun{
#'
#' # configure IMAP
#' library(mRpostman)
#' configure_imap(url="imaps://imap.gmail.com",
#'                username="your_gmail_user",
#'                password=rstudioapi::askForPassword()
#'               )
#'
#' select_folder(folder = "[Gmail]/Trash") %>%
#' result <- get_max_id(flag = "SEEN", by = "UID")
#'
#' }
#'
#' @export
#'
get_max_id <- function(flag, by = "MSN", retries = 2) {

  check_args_get_max_min_id(flag, by, retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  # forcing server url to the format we need -- removing unnecessary final slashe(s)
  #.. and doing URL encoding
  url <- utils::URLencode(gsub("/+$", "", IMAP_conn$imapconf$url))

  # isolating the handle
  h <- IMAP_conn$imapconf$conn_handle

  # adding the SEARCH id RETURN COUNT customrequest
  if (by == "UID") {
    tryCatch({
      curl::handle_setopt(
        handle = h,
        customrequest = paste0("UID SEARCH RETURN (MAX) ", flag))
    }, error = function(e){
      stop("The connection handle is dead. Please, configure a new IMAP connection with configure_imap().")
    })

  } else {
    tryCatch({
      curl::handle_setopt(
        handle = h,
        customrequest = paste0("SEARCH RETURN (MAX) ", flag))
    }, error = function(e){
      stop("The connection handle is dead. Please, configure a new IMAP connection with configure_imap().")
    })

  }

  # REQUEST
  response <- tryCatch({
    curl::curl_fetch_memory(url, handle = h)
  }, error = function(e){
    # print(e$message)
    response_error_handling(e$message[1]) # returns NULL for operation timeout: try reconnection
  })

  if (!is.null(response)) {
    response <- as.numeric(as.character(
      stringr::str_match_all(
        string = rawToChar(response$content),
        pattern = "MAX ([\\d]+)\r\n")[[1]][,2]
    ))


  } else { # it is not necessary to select again
    count_retries = 1 #the first try was already counted
    # FORCE appending fresh_connect
    # curl::handle_setopt(handle = h, fresh_connect = TRUE)

    while (is.null(response) && count_retries < retries) {
      count_retries = count_retries + 1
      # REQUEST
      response <- tryCatch({
        curl::curl_fetch_memory(url, handle = h)
      }, error = function(e){
        # print(e$message)
        response_error_handling(e$message[1]) # returns NULL for operation timeout: try reconnection
      })
    }

    if (!is.null(response)) {
      response <- as.numeric(as.character(
        stringr::str_match_all(
          string = rawToChar(response$content),
          pattern = "MAX ([\\d]+)\r\n")[[1]][,2]
      ))

    } else {
      stop('Request error: the server returned an error.')
    }

  }

  if (!(length(response) >  0)) {
    response = 0
  }

  names(response) <- c("MAX")
  # handle sanitizing
  rm(h)

  return(response)

}
