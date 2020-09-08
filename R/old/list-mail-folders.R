#' @title Listing Mail Folders v0.3.2-X
#'
#' @description List Folders from a mailbox.
#'
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
#' configure_imap(url="imaps://imap.gmail.com",
#'                username="your_gmail_user",
#'                password=rstudioapi::askForPassword()
#'               )
#'
#' # list mail folders
#' list_mail_folders()
#'
#' }
#' @export
#'
list_mail_folders <- function(retries = 2) {

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

  # forcing server url to the format we need -- removing unnecessary final slashe(s)
  #.. and doing URL encoding
  url <- utils::URLencode(gsub("/+$","", IMAP_conn$imapconf$url))

  # isolating the handle
  h <- IMAP_conn$imapconf$conn_handle

  tryCatch({
    curl::handle_setopt(h, customrequest = 'LIST "" *')
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
    # occurrences_splitted <- stringr::str_split(
    #   string = rawToChar(response$content),
    #   pattern = '\r\n\\*')

    # v0.3.2 - substituting stringr with base R
    occurrences_splitted <- strsplit(x = rawToChar(response$content),
                                     split = '\r\n\\*|\r\n')

    # folder_check_noselect <- do.call(
    #   stringr::str_detect, c(string = occurrences_splitted,
    #                          pattern = '\\\\Noselect')
    # )

    folder_check_noselect <- do.call(
      grepl, c(pattern = '\\\\Noselect', x = occurrences_splitted)
    )

    # occurrences_names <- stringr::str_match_all(
    #   string = rawToChar(response$content),
    #   pattern = '.*\" \"*(.*?)[(\\"\r\n)|(\r\n\\*)]')[[1]][,2] # more general
    # ok for Gmail, Yahoo, AOL and Yandex
    # Gmail, Yahoo, AOL - folder names RHS: (\\"\r\n)
    # Yandex - folder names RHS: (\r\n\\*)

    pattern = '\" (.*?)\r\n'

    m <- gregexpr(pattern, rawToChar(response$content))
    occurrences_names <- regmatches(rawToChar(response$content), m)
    occurrences_names <- lapply(occurrences_names, function(x) gsub('\" |\"', "", x))
    occurrences_names <- unlist(lapply(occurrences_names, function(x) gsub('\r\n.*$', "", x)))

    # folder_check_children <- do.call(
    #   stringr::str_detect, c(string = list(occurrences_names),
    #                          pattern = './.')
    # )

    # better identification of the hierarchy separator
    hierarchy_sep <- unlist(regmatches(occurrences_splitted[[1]][1],
                                regexec(' LIST \\(.*\\) (.*?) ',
                                         occurrences_splitted[[1]][1])
                                ))[2]
    # cleaning
    hierarchy_sep <- gsub('\\"', "", hierarchy_sep)

    pattern_hierarchy_sep = paste0('.', hierarchy_sep, '.')

    folder_check_children <- do.call(
      grepl, c(pattern = pattern_hierarchy_sep, x = list(occurrences_names))
    )

    # folder_check_children <- do.call(
    #   grepl, c(pattern = './.|.\\..', x = list(occurrences_names))
    # )

    # dropping type \Noselect
    folder_check_children <- folder_check_children[!folder_check_noselect]
    occurrences_names <- occurrences_names[!folder_check_noselect]

    # sanitizing
    rm(h)
    rm(response)
    rm(occurrences_splitted)

    # separate which ones are folderes and which are folderes/children
    final_output <- list(root = NULL, children = NULL)
    final_output$root <- occurrences_names[!folder_check_children]
    final_output$children <- occurrences_names[folder_check_children]

    # sanitizing
    rm(occurrences_names)
    rm(folder_check_children)
    rm(folder_check_noselect)

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
      occurrences_splitted <- strsplit(x = rawToChar(response$content),
                                       split = '\r\n\\*|\r\n')

      folder_check_noselect <- do.call(
        grepl, c(pattern = '\\\\Noselect', x = occurrences_splitted)
      )

      pattern = '\" (.*?)\r\n'

      m <- gregexpr(pattern, rawToChar(response$content))
      occurrences_names <- regmatches(rawToChar(response$content), m)
      occurrences_names <- lapply(occurrences_names, function(x) gsub('\" |\"', "", x))
      occurrences_names <- unlist(lapply(occurrences_names, function(x) gsub('\r\n.*$', "", x)))

      # better identification of the hierarchy separator
      hierarchy_sep <- unlist(regmatches(occurrences_splitted[[1]][1],
                                         regexec(' LIST \\(.*\\) (.*?) ',
                                                 occurrences_splitted[[1]][1])
      ))[2]
      # cleaning
      hierarchy_sep <- gsub('\\"', "", hierarchy_sep)

      pattern_hierarchy_sep = paste0('.', hierarchy_sep, '.')

      folder_check_children <- do.call(
        grepl, c(pattern = pattern_hierarchy_sep, x = list(occurrences_names))
      )

      # dropping type \Noselect
      folder_check_children <- folder_check_children[!folder_check_noselect]
      occurrences_names <- occurrences_names[!folder_check_noselect]

      # sanitizing
      rm(h)
      rm(response)
      rm(occurrences_splitted)

      # separate which ones are folderes and which are folderes/children
      final_output <- list(root = NULL, children = NULL)
      final_output$root <- occurrences_names[!folder_check_children]
      final_output$children <- occurrences_names[folder_check_children]

      # sanitizing
      rm(occurrences_names)
      rm(folder_check_children)
      rm(folder_check_noselect)

    } else {
      stop('Request error: the server returned an error.')
    }

  }

  return(final_output)

}
