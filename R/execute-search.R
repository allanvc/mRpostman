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

  # ESEARCH is an optional extension (RFC 4731). Only the esearch = TRUE path
  # relies on it; gate it here so any search_*() call fails early with a clear
  # message on a server that does not advertise ESEARCH.
  if (isTRUE(esearch)) {
    assert_capability(self, "ESEARCH", command = "search (esearch = TRUE)",
                      rfc = "RFC 4731", retries = retries)
  }

  # searching
  # REQUEST
  response <- tryCatch({
    curl::curl_fetch_memory(url, handle = handle)
  }, error = function(e) e)

  if (inherits(response, "error")) {
    server_reply <- last_server_error()
    if (!is.na(server_reply) && grepl("BADCHARSET", server_reply, fixed = TRUE)) {
      # the server does not accept the charset of the search term: retry with
      # the charsets it lists (or ISO-8859-1 / US-ASCII) in which the term is
      # representable. The rejection dropped the connection, so the folder is
      # re-selected before each attempt.
      alternatives <- charset_fallback_requests(customrequest, server_reply)
      response <- NULL
      for (alt in alternatives) {
        select_folder_int(self, name = self$con_params$folder, mute = TRUE, retries = 0)
        curl::handle_setopt(handle = handle, customrequest = alt)
        response <- tryCatch(curl::curl_fetch_memory(url, handle = handle),
                             error = function(e) NULL)
        if (!is.null(response)) {
          customrequest <- alt
          break
        }
      }
      if (is.null(response)) {
        stop(paste0("The server rejected the search charset (", server_reply,
                    ") and the search term cannot be represented in any ",
                    "character set it accepts."), call. = FALSE)
      }
    } else if (!is.na(server_reply) && grepl("Could not parse|parse error", server_reply, ignore.case = TRUE) &&
               any(as.integer(charToRaw(customrequest)) > 127L) &&
               "UTF8=ACCEPT" %in% toupper(get_server_capabilities(self, retries = retries))) {
      # the connection was replaced since UTF8=ACCEPT was enabled (Gmail drops
      # the connection after any rejected command): enable it again and retry
      self$enabled_epochs[["UTF8=ACCEPT"]] <- NULL
      ensure_utf8_enabled(self, toupper(get_server_capabilities(self, retries = retries)), retries)
      curl::handle_setopt(handle = handle, customrequest = customrequest)
      response <- tryCatch(curl::curl_fetch_memory(url, handle = handle),
                           error = function(e) response_error_handling(conditionMessage(e)))
    } else {
      response <- response_error_handling(conditionMessage(response))
    }
  }

  if (!is.null(response)) {
    if (isTRUE(esearch)) {
      pre_response <- parse_esearch_all(rawToChar(response$content))

    } else {
      pre_response <- parse_search_ids(rawToChar(response$content))

    }
    # ESEARCH condenses the response (e.g. "ALL 1:5,8"); parse_esearch_all()
    # expands the sequence-set without eval()-ing server-provided text.

    if (length(pre_response) > 0) {
      response <- pre_response
      rm(pre_response)

    } else if (grepl("\\* (ESEARCH|SEARCH)", rawToChar(response$content))) {
      # a confirmed search response with no ids: a legitimate empty result
      response <- integer(0)

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
        pre_response <- parse_esearch_all(rawToChar(response$content))

      } else {
        pre_response <- parse_search_ids(rawToChar(response$content))

      }
      # ESEARCH condenses the response (e.g. "ALL 1:5,8"); parse_esearch_all()
      # expands the sequence-set without eval()-ing server-provided text.

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
    warning(
    'The server has returned > 5000 results and may have truncated some lines.\n
    Check it by setting "verbose = TRUE".\n
    Consider increasing "buffersize" and/or setting "esearch = TRUE", if supported.'
    )
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
