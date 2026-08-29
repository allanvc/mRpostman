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

  if (is.na(self$con_params$folder)) {
    stop_no_folder()
  }

  # ESEARCH is an optional extension (RFC 4731). Only the esearch = TRUE path
  # relies on it; gate it here so any search call fails early with a clear
  # message on a server that does not advertise ESEARCH. The gate runs BEFORE
  # the command is set on the handle (inside imap_exec), so its CAPABILITY
  # fetch can never clobber the pending SEARCH.
  if (isTRUE(esearch)) {
    assert_capability(self, "ESEARCH", command = "search (esearch = TRUE)",
                      rfc = "RFC 4731", retries = retries)
  }

  parse_ids <- function(response) {
    content <- rawToChar(response$content)
    pre <- if (isTRUE(esearch)) parse_esearch_all(content) else parse_search_ids(content)
    # ESEARCH condenses the response (e.g. "ALL 1:5,8"); parse_esearch_all()
    # expands the sequence-set without eval()-ing server-provided text.
    if (length(pre) > 0) {
      pre
    } else if (grepl("\\* (ESEARCH|SEARCH)", content)) {
      # a confirmed search response with no ids: a legitimate empty result
      integer(0)
    } else {
      NA
    }
  }

  # first attempt: the raw error object is needed to drive the charset and
  # UTF8=ACCEPT recoveries, so this one bypasses imap_exec
  tryCatch({
    curl::handle_setopt(handle, customrequest = customrequest)
  }, error = function(e) {
    stop_dead_handle()
  })
  response <- tryCatch({
    curl::curl_fetch_memory(url, handle = handle)
  }, error = function(e) e)

  if (inherits(response, "error")) {
    server_reply <- last_server_error(self$con_debug$lines)
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
        stop_mrp(paste0("The server rejected the search charset (", server_reply,
                        ") and the search term cannot be represented in any ",
                        "character set it accepts."), "server_error",
                 server_reply = server_reply)
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
                           error = function(e) response_error_handling(conditionMessage(e), self))
    } else {
      response <- response_error_handling(conditionMessage(response), self)
    }
  }

  if (is.null(response)) {
    # retryable failure: the unified engine restores the session state
    # (ENABLEd extensions, the selected folder) and replays the command
    response <- imap_exec(self, customrequest, retries = retries,
                          needs_folder = TRUE, command = "SEARCH")$response
  }

  response <- parse_ids(response)
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

  return(response)

}
