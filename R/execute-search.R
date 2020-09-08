#' Execution engine for all the search commands
#' @param self The R6 connection object.
#' @param url A string containing the url from the \code{ImapCon$url} object.
#' @param handle A curl handle object with the custom request already defined.
#' @param customrequest A string containing the custom request to the server that
#'   will be added to the curl handle.
#' @param retries Number of attempts to connect and execute the command. Default
#'   is \code{1}.
#' @noRd
execute_search <- function(self, url, handle, customrequest, esearch, retries) {

  # previous folder selection checking
  # if (is.na(self$folder)) {
  #   stop('No folder previously selected.')
  # }
  assertthat::assert_that(
    !is.na(self$con_params$folder),
    msg='No folder previously selected.')

  # searching
  # REQUEST
  response <- tryCatch({
    curl::curl_fetch_memory(url, handle = handle)
  }, error = function(e){
    # print(e$message)
    response_error_handling(e$message[1])
  })

  if (!is.null(response)) {
    if (isTRUE(esearch)) {
      pre_response <- stringr::str_match_all(rawToChar(response$content), 'ALL (.*)')[[1]][,2]
      pre_response <- eval(parse(text = paste0("c(", pre_response, ")")))

    } else {
      pre_response <- as.numeric(
        as.character(
          stringr::str_extract_all(rawToChar(response$content), '\\d+')[[1]]
        )
      )

    }
    # note to self: changed the SEARCH METHOD FOR ESEARCH
    # that optimizes the response, but we need to use eval(parse(.))

    if (length(pre_response) > 0) {
      response <- pre_response
      rm(pre_response)

    } else {
      response = NA

    }

  } else {
    count_retries = 0
    # curl::handle_setopt(handle = h, fresh_connect = TRUE)

    # reselect the folder:
    select_folder_int(self, name = self$con_params$folder, mute = TRUE, retries = 0) # ok! v0.0.9
    # just to keep the folder selection in case of "BAD SEARCH not allowed now",
    # for example
    # this happens when we execute the search after a long period without
    # executing any command. It loses folder selection

    while (is.null(response) && count_retries < retries) {
      count_retries = count_retries + 1

      # reselect the folder:
      # select_folder_int(self, name = self$folder, silent = TRUE, retries = 1)
      # just to keep the folder selection in case of "BAD SEARCH not allowed now",
      # for example
      # this happens when we execute the search after a long period without
      # executing any command. It loses folder selection

      # reset customrequest in handle
      tryCatch({
        curl::handle_setopt(
          handle = handle,
          customrequest = customrequest)
      }, error = function(e){
        stop("The connection handle is dead. Please, configure a new IMAP connection with configure_imap().")
      })

      # REQUEST
      response <- tryCatch({
        curl::curl_fetch_memory(url, handle = handle)
      }, error = function(e){
        # print(e$message)
        response_error_handling(e$message[1])
      })
    }

    if (!is.null(response)) {
      if (isTRUE(esearch)) {
        pre_response <- stringr::str_match_all(rawToChar(response$content), 'ALL (.*)')[[1]][,2]
        pre_response <- eval(parse(text = paste0("c(", pre_response, ")")))

      } else {
        pre_response <- as.numeric(
          as.character(
            stringr::str_extract_all(rawToChar(response$content), '\\d+')[[1]]
          )
        )

      }
      # note to self: changed the SEARCH METHOD FOR ESEARCH
      # that optimizes the response, but we need to use eval(parse(.))

      if (length(pre_response) > 0) {
        response <- pre_response
        rm(pre_response)

      } else {
        response = NA

      }

    } else {
      # end reselecting the folder:
      # select_folder(name = IMAP_conn$imapconf$folder, silent = TRUE)
      # select_folder_int(self, name = self$folder, silent = TRUE, retries = 1)
      # just to keep the folder selection in case of BAD SEARCH not allowed now
      # this happens when we execute the search after a long period without
      # executing any command. It loses folder selection
      stop('Request error: the server returned an error.')
    }

  }
  # handle sanitizing
  rm(handle)
  response <- as.integer(as.character(response))

  # fix stripping
  response <- fix_search_stripping(response) # it does not have any effect on MS Exchange

  if (length(response) > 5000) {
    warning('The server has returned > 5000 results and may have truncated some lines.\n
    Check it by setting "verbose = TRUE" in ImapConf$new().\n
    Consider increasing "buffersize" and/or setting "esearch = TRUE", if supported.
    ')
  }

  # if (isTRUE(return_imapconf)) {
  #   final_output <- list("imapconf" = imapconf, "msg_id" = response)
  #   return(final_output)
  #
  # } else {
  #
  #   return(response)
  #
  # }
  if (self$con_params$verbose) {
    Sys.sleep(0.01)  # wait for the end of the client-server conversation
  }
  return(response)


}
