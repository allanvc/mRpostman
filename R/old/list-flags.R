#' @title List Flags of a Mail Folder v0.3.2-X
#'
#' @description List current and permanent flags in a selected mail folder.
#'
#' @param retries Number of attempts to connect and execute the command. Default
#'     is \code{2}.
#'
#' @return A \code{list} containing the current and the (\code{"PERMANENT"})
#'     flags in the selected mailbox.
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
#' # Select INBOX & list its flags
#' select_folder(name = "TAM")
#'
#' list_flags()
#'
#' }
#' @export
#'
list_flags <- function(retries = 2) {

  # check
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

  # isolating the handle
  h <- IMAP_conn$imapconf$conn_handle

  tryCatch({
    # adding the SELECT mbox customrequest parameter to handle -- will exhibit the flags
    curl::handle_setopt(handle = h, customrequest = paste0("SELECT ", folder))
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
    pattern = "(?<=FLAGS \\().+?(?=\\))" # using look operators
    # gets * FLAGS (...) and [PERMANENTFLAGS (...)]
    flags <- unlist(regmatches(rawToChar(response$headers),
                                  gregexpr(pattern,
                                           rawToChar(response$headers),
                                           perl=TRUE)))
    # flags <- gsub("\\\\", "", flags) # backslashes are symbols of system flags in IMAP
    # we cannot eliminate them
    # R uses \\

    if (length(flags) == 2) {
      all_flags <- flags[[1]]
      permanent_flags <- flags[[2]]

    } else { # for Sun iPlanet Messaging Server 5.2
      all_flags <- ""
      permanent_flags <- flags[[1]]
    }

  } else {

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

      pattern = "(?<=FLAGS \\().+?(?=\\))" # using look operators
      # gets * FLAGS (...) and [PERMANENTFLAGS (...)]
      flags <- unlist(regmatches(
        rawToChar(response$headers),
        gregexpr(pattern,
                 rawToChar(response$headers),
                 perl = TRUE)))

      # flags <- gsub("\\\\", "", flags) # backslashes are symbols of system flags in IMAP
      # we cannot eliminate them
      # R uses \\

      if (length(flags) == 2) {
        all_flags <- flags[[1]]
        permanent_flags <- flags[[2]]

      } else { # for Sun iPlanet Messaging Server 5.2
        all_flags <- ""
        permanent_flags <- flags[[1]]
      }


    } else {

      stop('Request error: the server returned an error.')

    }
  }

  flags_out <- list()
  flags_out$flags  <- unlist(strsplit(x = all_flags, split = " "))
  flags_out$permanent_flags <- unlist(strsplit(x = permanent_flags, split = " "))

  return(flags_out)
}
