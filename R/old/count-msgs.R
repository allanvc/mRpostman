#' @title Count the Number of Messages
#'
#' @description Counts the number of messages with a respective flag in a folder.
#'
#' @param flag Optional parameter that adds a flag filter to the search. Use
#'     \code{\link{list_flags}} to list the flags in a selected mail folder.
#'     Default is \code{NULL}.
#' @param by Which id (\code{MSN} or \code{UID}) to use when searching for
#'     messages. \code{"MSN"} or message sequence number is a message's relative
#'     position to the oldest message in the mailbox. It may change after deleting
#'     or moving messages. For instance, if a message is deleted, sequence
#'     numbers are reordered to fill the gap. The \code{"UID"} or unique identifier
#'     is always the same during the life cycle of a message. Default is
#'     \code{"MSN"}.
#' @param retries Number of attempts to connect and execute the command. Default
#'     is \code{2}.
#'
#' @return A numeric \code{vector} of length \code{1} containing the number of
#'     messages that meet the specified criteria.
#'
#' @family miscellaneous
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
#' select_folder(folder = "TAM")
#'
#' results <- select_folder(folder = "INBOX") %>%
#'     count_msgs(by = "UID", flag = "SEEN")
#'
#' }
#'
#' @export
#'
count_msgs <- function(flag, by = "MSN", retries = 2) {

  # checks
  assertthat::assert_that(
    any(
      by == "MSN",
      by == "UID"
    ),
    msg='"by" must be set as "MSN" or "UID".')

  assertthat::assert_that(
    is.character(flag),
    msg='"flag" argument must be a character.')

  assertthat::assert_that(
    is.numeric(retries),
    assertthat::validate_that(retries >= 1),
    msg='"retries" must be an integer equal or greater than 1.')

  if (retries%%1 != 0) {
    warning('only the integer part of "retries" will be used.')
  }

  # forcing retries as an integer
  retries <- as.integer(retries)

  # forcing server url to the format we need -- removing unnecessary final slashe(s)
  #.. and doing URL encoding
  url <- utils::URLencode(gsub("/+$", "", IMAP_conn$imapconf$url))

  # isolating the handle
  h <- IMAP_conn$imapconf$conn_handle

  # quoting to guarantee folder with more than one name
  folder <- adjust_folder_name(IMAP_conn$imaconf$folder) # there is a reselection in the end

  # adding the SEARCH id RETURN COUNT customrequest
  if (by == "UID") {

    tryCatch({
      curl::handle_setopt(
        handle = h,
        customrequest = paste0("UID SEARCH RETURN (COUNT) ", flag))
    }, error = function(e){
      stop("The connection handle is dead. Please, configure a new IMAP connection with configure_imap().")
    })

  } else {

    tryCatch({
      curl::handle_setopt(
        handle = h,
        customrequest = paste0("SEARCH RETURN (COUNT) ", flag))
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
        pattern = "COUNT ([\\d]+)\r\n")[[1]][,2]
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
          pattern = "COUNT ([\\d]+)\r\n")[[1]][,2]
      ))

    } else {
      stop('Request error: the server returned an error.')
    }

  }

  names(response) <- c("COUNT")
  # handle sanitizing
  rm(h)
  return(response)

}
