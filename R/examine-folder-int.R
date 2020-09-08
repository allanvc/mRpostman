#' Examine the number of messages in a mail folder (INTERNAL HELPER)
#' @param name A string containing the name of an existing mail folder on the
#'   user's mailbox. If no name is passed, the command will be executed using the
#'   previously selected mail folder name.
#' @param retries Number of attempts to connect and execute the command.
#'   Default is \code{1}.
#' @noRd
examine_folder_int <- function(self, name, retries) {


  # check
  if (!is.null(name)) { # name can also be NULL here
    # if (!is.character(name)) {
    #   stop('"name" must be of type character or NULL.')
    # }
    assertthat::assert_that(
      is.character(name),
      msg='"name" must be of type character or NULL.')
  } else {
    # previous folder selection checking
    # if (is.na(self$folder)) {
    #   stop('No folder previously selected.')
    # }
    assertthat::assert_that(
      !is.na(self$con_params$folder),
      msg='No folder previously selected.')
  }

  check_args(retries = retries) # we have to pass
  #.. the argg as arg = arg, in order to the check_argg capture the names

  # forcing retries as an integer
  retries <- as.integer(retries)

  if (is.null(name)) {

    # folder <- self$folder
    folder <- adjust_folder_name(self$con_params$folder)

  } else {

    assertthat::assert_that(
      is.character(name),  # not mandatory
      msg='"name" must be of type character or NULL. Use list_mail_folders().')

    folder <- adjust_folder_name(name) # only here
  }

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  tryCatch({
    # adding the EXAMINE folder customrequest parameter
    curl::handle_setopt(handle = h, customrequest = paste0("EXAMINE ", folder))
  }, error = function(e){
    stop("The connection handle is dead. Please, configure a new IMAP connection with ImapCon$new().")
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
    count_retries = 0 #the first try doesnt count
    # FORCE appending fresh_connect
    # curl::handle_setopt(handle = h, fresh_connect = TRUE)

    # reselect the folder:
    # select_folder_int(self, name = self$folder, silent = TRUE) # try to recover the selected folder
    # aqui nao eh necessario
    # quando, for usar, extrair a lista e manipular

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
  if (self$con_params$verbose) {
    Sys.sleep(0.01)  # wait for the end of the client-server conversation
  }
  return(exam_out)

}
