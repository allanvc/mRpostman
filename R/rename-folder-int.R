#' Rename a mail folder (INTERNAL HELPER)
#' @param name A string containing the name of the new mail folder to be
#'   renamed. If no name is passed, the command will be executed using the
#'   previously selected mail folder name.
#' @param new_name A string containing the new name to be assigned.
#' @param reselect A logical. If \code{TRUE}, calls
#'   \code{select_folder(name = to_folder)} under the hood before returning
#'   the output. Default is \code{TRUE}.
#' @param mute A \code{logical}. Provides a confirmation message if the
#'   command is successfully executed. Default is \code{FALSE}.
#' @param retries Number of attempts to connect and execute the command.
#'   Default is \code{1}.
#' @noRd
rename_folder_int <- function(self, name, new_name, reselect, mute, retries) {

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

  check_args(new_name = new_name, reselect = reselect, mute = mute,
             retries = retries) # we have to pass
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

  # quoting to guarantee the correct behaviour when folders have more than one name
  new_name2 <- adjust_folder_name(new_name) # new object, because we'll assign..
  # to imapconf (without \"INBOX\")

  #folder <- adjust_folder_name(IMAP_conn$imapconf$folder) # vai retornar lista agora

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

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

    if (is.null(response)) {

      stop('Request error: the server returned an error.')

    } else { # v0.3.2

      # reselecting
      if (isTRUE(reselect)) {
        # imapconf$folder = new_name
        out <- select_folder_int(self, name = new_name, mute = mute, retries = 1)
      } else {
        out <- new_name
      }

      if (!mute) {

        if (self$con_params$verbose) {
          Sys.sleep(0.01)  # wait for the end of the client-server conversation
        }

        if (is.null(name)) {

          cat(paste0("::mRpostman: ", '"', self$con_params$folder, '"',
                     " renamed to ", '"', new_name, '".')) # v0.3.2

        } else {

          cat(paste0("::mRpostman: ", '"', name, '"',
                     " renamed to ", '"', new_name, '".')) # v0.3.2
        }

      }

    }

  } else {

    # reselecting
    if (isTRUE(reselect)) {
      # imapconf$folder = new_name
      out <- select_folder_int(self, name = new_name, mute = mute, retries = 1)
    } else {
      out <- new_name
    }

    if (!mute) {

      if (self$con_params$verbose) {
        Sys.sleep(0.01)  # wait for the end of the client-server conversation
      }

      if (is.null(name)) {

        cat(paste0("::mRpostman: ", '"', self$con_params$folder, '"',
                   " renamed to ", '"', new_name, '".')) # v0.3.2

      } else {

        cat(paste0("::mRpostman: ", '"', name, '"',
                   " renamed to ", '"', new_name, '".')) # v0.3.2
      }

    }

  }

  # handle sanitizing
  rm(h)

  return(out)

}
