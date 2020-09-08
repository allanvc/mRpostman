#' List flags in a selected mail folder (INTERNAL HELPER)
#' @param retries Number of attempts to connect and execute the command.
#'   Default is \code{1}.
#' @noRd
list_flags_int <- function(self, retries) {

  assertthat::assert_that(
    !is.na(self$con_params$folder),
    msg='No folder previously selected.')

  check_args(retries = retries) # we have to pass
  #.. the argg as arg = arg, in order to the check_argg capture the names

  # forcing retries as an integer
  retries <- as.integer(retries)

  folder <- adjust_folder_name(self$con_params$folder)

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  tryCatch({
    # adding the SELECT mbox customrequest parameter to handle -- will exhibit the flags
    curl::handle_setopt(handle = h, customrequest = paste0("SELECT ", folder))
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
    pattern = "(?<=FLAGS \\().+?(?=\\))" # using look operators
    # gets * FLAGS (...) and [PERMANENTFLAGS (...)]
    flags <- unlist(regmatches(rawToChar(response$headers),
                                  gregexpr(pattern,
                                           rawToChar(response$headers),
                                           perl=TRUE)))

    # check if custom flags are allowed: v0.9.0
    custom_flags_check <- any(grepl(pattern = "\\\\\\*", flags))

    flags <- gsub("\\\\\\*", "", flags) # backslashes are symbols of system flags in IMAP
    # we cannot eliminate them
    # R uses \\

    if (length(flags) == 2) {
      all_flags <- flags[[1]]
      permanent_flags <- flags[[2]]
      custom_flags_allowed <- custom_flags_check #v0.9.0

    } else { # for Sun iPlanet Messaging Server 5.2
      all_flags <- ""
      permanent_flags <- flags[[1]]
      custom_flags_allowed <- custom_flags_check #v0.9.0
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


      # check if custom flags are allowed: v0.9.0
      custom_flags_check <- any(grepl(pattern = "\\\\\\*", flags))

      # flags <- gsub("\\\\", "", flags) # backslashes are symbols of system flags in IMAP
      # we cannot eliminate them
      # R uses \\
      flags <- gsub("\\\\\\*", "", flags) # backslashes are symbols of system flags in IMAP

      if (length(flags) == 2) {
        all_flags <- flags[[1]]
        permanent_flags <- flags[[2]]
        custom_flags_allowed <- custom_flags_check #v0.9.0

      } else { # for Sun iPlanet Messaging Server 5.2
        all_flags <- ""
        permanent_flags <- flags[[1]]
        custom_flags_allowed <- custom_flags_check #v0.9.0
      }


    } else {

      stop('Request error: the server returned an error.')

    }
  }

  flags_out <- list()
  flags_out$flags  <- unlist(strsplit(x = all_flags, split = " "))
  flags_out$permanent_flags <- unlist(strsplit(x = permanent_flags, split = " "))
  flags_out$custom_flags_allowed <- custom_flags_allowed
  if (self$con_params$verbose) {
    Sys.sleep(0.01)  # wait for the end of the client-server conversation
  }
  return(flags_out)
}
