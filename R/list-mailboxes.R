#' @title Listing Mailboxes
#'
#' @description List Mailboxes from IMAP server.
#'
#' @param imapconf An object of class \code{imapconf} generated with
#'     \link{configure_imap}.
#' @param retries Number of attempts to connect and execute the command. Default
#'     is \code{2}.
#'
#' @return A \code{list} containing the Mailboxes (root and children).
#'
#' @family mailbox commands
#'
#' @examples
#' \dontrun{
#'
#' # configure IMAP
#' library(mRpostman)
#' imapconf <- configureIMAP(url="imaps://imap.gmail.com",
#'                           username="your_gmail_user",
#'                           password=rstudioapi::askForPassword()
#'                          )
#'
#' # list mailboxes
#' results <- imapconf %>%
#'   list_mailboxes()
#'
#' }
#' @export
#'
list_mailboxes <- function(imapconf, retries = 2) {

  # check
  assertthat::assert_that(
    assertthat::validate_that(class(imapconf) == "imapconf"),
    msg='"imapconf" must be of class "imapconf". Use configIMAP() to create a valid "imapconf" object.')

  assertthat::assert_that(
    is.numeric(retries),
    assertthat::validate_that(retries >= 1),
    msg='"retries" must be an integer equal or greater than 1.')

  if (retries%%1 != 0) {
    warning('only the integer part of "retries" will be used.')
  }

  # forcing server url to the format we need -- removing unnecesary final slashe(s)
  imapconf$url <- gsub("/+$","", imapconf$url)

  # forcing retries as an integer
  retries <- as.integer(retries)

  # config handle
  h <- config_handle(imapconf)

  response <- tryCatch({
    curl::curl_fetch_memory(imapconf$url, handle = h)
    }, error = function(e) {
      return(NULL)
    })

  if (!is.null(response)) {
    occurrencies_splitted <- stringr::str_split(
      string = rawToChar(response$content),
      pattern = '\r\n\\*')

    mbox_check_noselect <- do.call(
      stringr::str_detect, c(string = occurrencies_splitted,
                             pattern = '\\\\Noselect')
    )

    occurrencies_names <- stringr::str_match_all(
      string = rawToChar(response$content),
      pattern = '.*\" \"*(.*?)\\"\r\n')[[1]][,2] # more general

    mbox_check_children <- do.call(
      stringr::str_detect, c(string = list(occurrencies_names),
                             pattern = './.')
    )

    # dropping type \Noselect
    mbox_check_children <- mbox_check_children[!mbox_check_noselect]
    occurrencies_names <- occurrencies_names[!mbox_check_noselect]

    # sanitizing
    rm(h)
    rm(response)
    rm(occurrencies_splitted)

    # separate which ones are mboxes and which are mboxes/children
    final_output <- list(root=NULL, children=NULL)
    final_output$root <- occurrencies_names[!mbox_check_children]
    final_output$children <- occurrencies_names[mbox_check_children]

    # sanitizing
    rm(occurrencies_names)
    rm(mbox_check_children)
    rm(mbox_check_noselect)

  } else {
    count_retries = 1 #the first try was already counted
    # FORCE appending fresh_connect
    curl::handle_setopt(handle = h, fresh_connect = TRUE)

    while (is.null(response) && count_retries < retries) {
      count_retries = count_retries+1
      response <- tryCatch({
        curl::curl_fetch_memory(imapconf$url, handle = h)

      }, error = function(e) {
        return(NULL)
      })
    }

    if (!is.null(response)) {
      occurrencies_splitted <- stringr::str_split(
        string = rawToChar(response$content),
        pattern = '\r\n\\*')

      mbox_check_noselect <- do.call(
        stringr::str_detect, c(string = occurrencies_splitted,
                               pattern = '\\\\Noselect')
      )

      occurrencies_names <- stringr::str_match_all(
        string = rawToChar(response$content),
        pattern = ".*/\" \"*(.*?) *\"\r.*")[[1]][,2]

      mbox_check_children <- do.call(
        stringr::str_detect, c(string = list(occurrencies_names),
                               pattern = './.')
      )

      # dropping type \Noselect
      mbox_check_children <- mbox_check_children[!mbox_check_noselect]
      occurrencies_names <- occurrencies_names[!mbox_check_noselect]

      # sanitizing
      rm(h)
      rm(response)
      rm(occurrencies_splitted)

      # separate which ones are mboxes and which are mboxes/children
      final_output <- list(root=NULL, children=NULL)
      final_output$root <- occurrencies_names[!mbox_check_children]
      final_output$children <- occurrencies_names[mbox_check_children]

      # sanitizing
      rm(occurrencies_names)
      rm(mbox_check_children)
      rm(mbox_check_noselect)

    } else {
      stop('An error ocurred while connecting. Please check the following and/or try again:\n
           - your internet connection status;\n
           - if your imapconf options are valid.'
          )
    }

  }

  return(final_output)

}
