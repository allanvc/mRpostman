#' @title Mail Folder Creation v0.3.2
#'
#' @description Create a new mail folder on the user mailbox.
#'
#' @param name A string containing the name of the folder to be created.
#' @param retries Number of attempts to connect and execute the command. Default
#'     is \code{2}.
#'
#' @return The (invisible) \code{imaconf} object with the selected mailbox added to it.
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
#' # Create a new folder
#' create_folder(name = "New Folder")
#'
#' }
#' @export
#'
create_folder <- function(name, retries = 2) {

  # check
  assertthat::assert_that(
    is.character(name),
    msg='"name" must be of type character')

  assertthat::assert_that(
    is.numeric(retries),
    assertthat::validate_that(retries >= 1),
    msg='"retries" must be an integer equal or greater than 1.')

  if (retries%%1 != 0) {
    warning('only the integer part of "retries" will be used.')
  }

  # forcing retries as an integer
  retries <- as.integer(retries)

  # imapconf$folder <- folder # only here

  name2 <- adjust_folder_name(name)

  # forcing server url to the format we need -- removing unnecessary final slashe(s)
  #.. and doing URL encoding
  url <- utils::URLencode(gsub("/+$", "", IMAP_conn$imapconf$url))

  # forcing retries as an integer
  retries <- as.integer(retries)

  # isolating the handle
  h <- IMAP_conn$imapconf$conn_handle

  tryCatch({
    curl::handle_setopt(h, customrequest = paste0('CREATE ', name2))
  }, error = function(e){
    stop("The connection handle is dead. Please, configure a new IMAP connection with configure_imap().")
  })

  response <- tryCatch({
    curl::curl_fetch_memory(url, handle = h)
  }, error = function(e){
    # print(e$message)
    response_error_handling(e$message[1]) # returns NULL for operation timeout: try reconnection
  })

  if(is.null(response)){

    count_retries = 1 #the first try was already counted

    # FORCE appending fresh_connect
    # curl::handle_setopt(handle = h, fresh_connect = TRUE) # parece que nao precisa, mas vamos deixar

    while (is.null(response) && count_retries < retries) {
      count_retries = count_retries + 1

      response <- tryCatch({
        curl::curl_fetch_memory(url, handle = h)
      }, error = function(e){
        # print(e$message)
        response_error_handling(e$message[1]) # returns NULL for operation timeout: try reconnection
      })

    }

    if (is.null(response)) {

      stop('Request error: the server returned an error.')

    } else { # v0.3.2
      cat(paste0("\n::mRpostman: folder ", '"', name, '"', " created\n")) # v0.3.2
    }

  } else {
    cat(paste0("\n::mRpostman: folder ", '"', name, '"', " created\n")) # v0.3.2
  }

  invisible(0L)

}
