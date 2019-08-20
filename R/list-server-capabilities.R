#' @title IMAP Server Capabilities
#'
#' @description Lists IMAP server's capabilities.
#'
#' @param imapconf An object of class \code{imapconf} generated with
#'     \link{configure_imap}.
#' @param retries Number of attempts to connect and execute the command. Default
#'     is \code{2}.
#'
#' @return A character \code{vector} containing server's IMAP capacbilities.
#'
#' @family mailbox commands
#'
#' @examples
#' \dontrun{
#'
#' # configure IMAP
#' #' library(mRpostman)
#' imapconf <- configureIMAP(url="imaps://imap.gmail.com",
#'                           username="your_gmail_user",
#'                           password=rstudioapi::askForPassword()
#'                          )
#'
#' # list server's capabilities
#' results <- imapconf %>%
#'   list_server_capabilities()
#'
#' }
#'
#' @export
#'
list_server_capabilities <- function(imapconf, retries = 2) {

  # check
  assertthat::assert_that(
    assertthat::validate_that(class(imapconf) == "imapconf"),
    msg='"imapconf" must be of class "imapconf". Use configIMAP() to create a valid "imapconf" object.')

  assertthat::assert_that(
    is.numeric(retries),
    assertthat::validate_that(retries >= 1),
    msg='"retries" must be an integer equal or greater than 1.')

  if(retries%%1 != 0){
    warning('only the integer part of "retries" will be used.')
  }

  # forcing server url to the format we need -- removing unnecesary final slashe(s)
  imapconf$url <- gsub("/+$","", imapconf$url)

  # forcing retries as an integer
  retries <- as.integer(retries)

  # config handle
  new_imapconf <- imapconf
  new_imapconf$fresh_connect = TRUE # need a fresh connect to get server capabilities
  h <- config_handle(new_imapconf)

  response <- tryCatch({
    curl::curl_fetch_memory(new_imapconf$url, handle = h)
  }, error = function(e){
    return(NULL)
  })

  if (!is.null(response)) {
    server_capabilities <- stringr::str_split(
      stringr::str_match_all( # only the second matching (after authenticating)
      string = rawToChar(response$headers), # diff to listMailboxes -- parse headers
      pattern = '\r\n\\* CAPABILITY (.*?)\r\n')[[1]][2,2], " ")[[1]]

    # sanitizing
    rm(h)
    rm(response)

  } else {
    count_retries = 1 #the first try was already counted
    # FORCE appending fresh_connect
    curl::handle_setopt(handle = h, fresh_connect = TRUE)

    while (is.null(response) && count_retries < retries) {
      count_retries = count_retries+1
      response <- tryCatch({
        curl::curl_fetch_memory(new_imapconf$url, handle = h)

      }, error = function(e) {
        return(NULL)
      })
    }

    if (!is.null(response)) {
      server_capabilities <- stringr::str_split(
        stringr::str_match_all( # only the second matching (after authenticating)
          string = rawToChar(response$headers), # diff to listMailboxes -- parse headers
          pattern = '\r\n\\* CAPABILITY (.*?)\r\n')[[1]][2,2], " ")[[1]]

      # sanitizing
      rm(h)
      rm(response)


    } else {
      stop('An error ocurred while connecting. Please check the following and/or try again:\n
           - your internet connection status;\n
           - if your imapconf options are valid.'
      )
    }

  }

  return(server_capabilities)

}
