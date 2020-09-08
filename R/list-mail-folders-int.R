#' @description List mail folders in a mailbox (INTERNAL HELPER)
#' @param retries Number of attempts to connect and execute the command.
#' @noRd
list_mail_folders_int <- function(self, retries) {

  check_args(retries = retries) # we have to pass
  #.. the argg as arg = arg, in order to the check_argg capture the names

  # forcing retries as an integer
  retries <- as.integer(retries)

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  tryCatch({
    curl::handle_setopt(h, customrequest = 'LIST "" *')
  }, error = function(e){
    stop("The connection handle is dead. Please, configure a new IMAP connection with ImapConf$new().")
  })

  response <- tryCatch({
    curl::curl_fetch_memory(url, handle = h)
  }, error = function(e){
    # print(e$message)
    response_error_handling(e$message[1])
  })

  if (!is.null(response)) {

    # v0.3.2 - substituting stringr with base R
    occurrences_splitted <- strsplit(x = rawToChar(response$content),
                                     split = '\r\n\\*|\r\n')

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


    # better identification of the hierarchy separator v0.3.2
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

    # separate which ones are folders and which are folders/children
    final_output <- list(root = NULL, children = NULL)
    final_output$root <- occurrences_names[!folder_check_children]
    final_output$children <- occurrences_names[folder_check_children]

    # sanitizing
    rm(occurrences_names)
    rm(folder_check_children)
    rm(folder_check_noselect)

  } else {
    count_retries = 0 #the first try doesnt count

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

  if (self$con_params$verbose) {
    Sys.sleep(0.01)  # wait for the end of the client-server conversation
  }
  return(final_output)

}
