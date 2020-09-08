#' @title Examining Selected Mail Folder v0.3.2-X (sringr -> base R)
#'
#' @description Retrieve the "RECENT" and the total number ("EXISTS") of
#'     messages in a selected mail folder.
#'
#' @param retries Number of attempts to connect and execute the command. Default
#'     is \code{2}.
#'
#' @return A \code{vector} containing the (\code{"EXISTS"}) and the
#'     (\code{"RECENT"}) number of messages in the selected folder.
#'
#' @family mailbox commands
#'
#' @examples
#' \dontrun{
#'
#' # configure IMAP
#' library(mRpostman)
#' configure_imap(url="imaps://imap.gmail.com",
#'                username="your_gmail_user",
#'                password=rstudioapi::askForPassword()
#'               )
#'
#' select_folder(name = "TAM")
#' # examine the selected folder:
#' examine_folder()
#'
#' }
#' @export
#'
examine_folder <- function(retries = 2) {

  assertthat::assert_that(
    is.numeric(retries),
    assertthat::validate_that(retries >= 1),
    msg='"retries" must be an integer equal or greater than 1.')

  if (retries%%1 != 0) {
    warning('only the integer part of "retries" will be used.')
  }

  # forcing retries as an integer
  retries <- as.integer(retries)

  folder <- adjust_folder_name(IMAP_conn$imapconf$folder)

  # forcing server url to the format we need -- removing unnecessary final slashe(s)
  #.. and doing URL encoding
  url <- utils::URLencode(gsub("/+$", "", IMAP_conn$imapconf$url))

  # forcing retries as an integer
  retries <- as.integer(retries)

  # isolating the handle
  h <- IMAP_conn$imapconf$conn_handle

  tryCatch({
    # adding the EXAMINE folder customrequest parameter
    curl::handle_setopt(handle = h, customrequest = paste0("EXAMINE ", folder))
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

    # v0.3.2: from stringr to base R
    pattern = "([\\d]+)(?= EXISTS| RECENT)" # using look behind operator (?=)

    exam_out <- unlist(regmatches(rawToChar(response$headers),
                           gregexpr(pattern,
                                    rawToChar(response$headers),
                                    perl = TRUE)))

  } else { # it is not necessary to select again
    count_retries = 1 #the first try was already counted
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

    if (!is.null(response)) {
      pattern = "([\\d]+)(?= EXISTS| RECENT)" # using look behind operator (?=)

      exam_out <- unlist(regmatches(rawToChar(response$headers),
                             gregexpr(pattern,
                                      rawToChar(response$headers),
                                      perl = TRUE)))

    } else {
      stop('Request error: the server returned an error.')
    }

  }

  exam_out <- as.numeric(as.character(exam_out))
  names(exam_out) <- c("EXISTS", "RECENT")

  # handle sanitizing
  rm(h)
  return(exam_out)

}
