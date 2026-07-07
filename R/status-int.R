#' Request the status of a mail folder without selecting it (INTERNAL HELPER)
#' @param name A string containing the name of an existing mail folder on the
#'   user's mailbox. If no name is passed, the command will be executed using
#'   the previously selected mail folder name.
#' @param items A character vector with the STATUS data items to request. Must
#'   be a subset of "MESSAGES", "RECENT", "UIDNEXT", "UIDVALIDITY", "UNSEEN".
#' @param retries Number of attempts to connect and execute the command.
#'   Default is \code{1}.
#' @noRd
status_int <- function(self, name, items, retries) {

  # check
  if (!is.null(name)) { # name can also be NULL here
    assertthat::assert_that(
      is.character(name),
      msg='"name" must be of type character or NULL.')
  } else {
    # previous folder selection checking
    assertthat::assert_that(
      !is.na(self$con_params$folder),
      msg='No folder previously selected.')
  }

  # items check
  assertthat::assert_that(
    is.character(items),
    msg='"items" must be a character vector. See the valid STATUS data items.')

  items <- toupper(items)

  valid_items <- c("MESSAGES", "RECENT", "UIDNEXT", "UIDVALIDITY", "UNSEEN")

  assertthat::assert_that(
    all(items %in% valid_items),
    msg=paste0('"items" must be a subset of: ',
               paste(valid_items, collapse = ", "), '.'))

  check_args(retries = retries) # we have to pass
  #.. the argg as arg = arg, in order to the check_argg capture the names

  # forcing retries as an integer
  retries <- as.integer(retries)

  if (is.null(name)) {

    folder <- adjust_folder_name(self$con_params$folder)

  } else {

    folder <- adjust_folder_name(name) # only here
  }

  items_str <- paste0("(", paste(items, collapse = " "), ")")

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  tryCatch({
    # adding the STATUS folder customrequest parameter
    curl::handle_setopt(handle = h,
                        customrequest = paste0("STATUS ", folder, " ", items_str))
  }, error = function(e){
    stop("The connection handle is dead. Please, configure a new IMAP connection with configure_imap().")
  })

  response <- tryCatch({
    curl::curl_fetch_memory(url, handle = h)
  }, error = function(e){
    # print(e$message)
    response_error_handling(e$message[1])
  })

  if (is.null(response)) { # it is not necessary to select again
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

  # the untagged "* STATUS" line may arrive via headers or content
  resp_char <- paste(rawToChar(response$headers), rawToChar(response$content))
  status_out <- parse_status_counts(resp_char)

  # handle sanitizing
  rm(h)
  if (self$con_params$verbose) {
    Sys.sleep(0.01)  # wait for the end of the client-server conversation
  }
  return(status_out)

}
