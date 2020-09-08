#' @title Renaming Mail Folder
#'
#' @description Rename a mail folder.
#'
#' @param new_name A string containing the new name to be set.
#' @param reselect A logical. If \code{TRUE}, calls \code{select_folder(name = to_folder)}
#'     under the hood before returning the output. Default is \code{TRUE}.
#' @param retries Number of attempts to connect and execute the command. Default
#'     is \code{2}.
#'
#' @inherit select_folder return
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
#' # rename mailbox "Sent"
#' select_folder(name = "Sent")
#' rename_folder(new_name = "Sent Mail")
#'
#' }
#' @export
#'
rename_folder <- function(new_name, reselect = TRUE, retries = 2) {

  # checks
  assertthat::assert_that(
    is.character(new_name),
    msg='"new_name" must be of type character.')

  assertthat::assert_that(
    is.logical(reselect),
    msg='"reselect" must be a logical.')

  assertthat::assert_that(
    is.numeric(retries),
    assertthat::validate_that(retries >= 1),
    msg='"retries" must be an integer equal or greater than 1.')

  if (retries%%1 != 0) {
    warning('only the integer part of "retries" will be used.')
  }

  # quoting to guarantee the correct behaviour when folders have more than one name
  new_name2 <- adjust_folder_name(new_name) # new object, because we'll assign..
  # to imapconf (without \"INBOX\")

  folder <- adjust_folder_name(IMAP_conn$imapconf$folder)

  # forcing server url to the format we need -- removing unnecessary final slashe(s)
  #.. and doing URL encoding
  url <- utils::URLencode(gsub("/+$", "", IMAP_conn$imapconf$url))

  # forcing retries as an integer
  retries <- as.integer(retries)

  # isolating the handle
  h <- IMAP_conn$imapconf$conn_handle

  tryCatch({
    # adding the RENAME folder customrequest parameter
    curl::handle_setopt(handle = h, customrequest = paste0("RENAME ",
                                                           folder, " ",
                                                           new_name2))
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
    # it is not necessary to select again
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

    if (is.null(response)) {
      stop('Request error: the server returned an error.')
    } else { # v0.3.2
      cat(paste0("::mRpostman: ", '"', IMAP_conn$imapconf$folder, '"',
                 " renamed to ", '"', new_name, '"')) # v0.3.2
    }

  } else {
    cat(paste0("::mRpostman: ", '"', IMAP_conn$imapconf$folder, '"',
               " renamed to ", '"', new_name, '"')) # v0.3.2
  }

  # handle sanitizing
  rm(h)

  # reselecting
  if (isTRUE(reselect)) {

    # imapconf$folder = new_name

    select_folder(name = new_name)


  }

  invisible(0L)

}
