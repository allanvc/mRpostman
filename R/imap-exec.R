# The single execution choke point of the libcurl transport. Setting the
# command on the shared handle and fetching are done back to back, with
# nothing in between, so no capability gate or state fetch can overwrite the
# pending command (the bug class behind several historical fixes). The retry
# policy lives here, once: on a retryable failure the session state (ENABLEd
# extensions, the selected folder) is restored before the command is replayed,
# because a reconnection loses both.

#' Restore the per-connection session state after a reconnection (INTERNAL)
#'
#' Re-enables every extension previously switched on with ENABLE (a no-op for
#' extensions whose connection epoch is still current) and re-selects the
#' selected folder, in this order, since ENABLE requires the authenticated,
#' not-selected state.
#' @noRd
restore_session_state <- function(self) {
  exts <- names(self$enabled_epochs)
  if (length(exts) > 0) {
    caps <- toupper(get_server_capabilities(self, retries = 0))
    for (ext in exts) {
      tryCatch(ensure_enabled(self, ext, caps, retries = 0),
               error = function(e) NULL)
    }
  }
  if (!is.na(self$con_params$folder)) {
    select_folder_int(self, name = self$con_params$folder, mute = TRUE,
                      retries = 0)
  }
  invisible(NULL)
}

#' Issue one IMAP command on the shared libcurl handle (INTERNAL ENGINE)
#'
#' @param self The R6 connection object.
#' @param customrequest The IMAP command to issue.
#' @param retries Number of additional attempts after a retryable failure.
#' @param needs_folder If \code{TRUE}, fails upfront (classed
#'   \code{mRpostman_state_error}) unless a folder is selected, and restores
#'   the selection before every retry.
#' @param command Optional human-readable command name for error messages.
#' @return A \code{list} with the raw curl \code{response} and \code{text},
#'   the response's header and body callbacks pasted with CRLF (libcurl may
#'   deliver untagged lines through either).
#' @noRd
imap_exec <- function(self, customrequest, retries = 1, needs_folder = FALSE,
                      command = NULL) {

  if (isTRUE(needs_folder) && is.na(self$con_params$folder)) {
    stop_no_folder()
  }

  retries <- as.integer(retries)
  url <- self$con_params$url
  h <- self$con_handle

  attempt <- 0L
  response <- NULL
  while (is.null(response) && attempt <= retries) {
    if (attempt > 0L) {
      # a retryable failure usually means the connection was replaced:
      # restore what the session had before replaying the command
      restore_session_state(self)
    }
    attempt <- attempt + 1L

    # set + fetch, atomically: nothing may run between these two calls
    tryCatch({
      curl::handle_setopt(h, customrequest = customrequest)
    }, error = function(e) {
      stop_dead_handle()
    })
    response <- tryCatch({
      curl::curl_fetch_memory(url, handle = h)
    }, error = function(e) {
      response_error_handling(e$message[1], self) # NULL = retryable
    })
  }

  if (is.null(response)) {
    stop_request_failed(self, command)
  }

  list(response = response,
       text = paste(rawToChar(response$headers), rawToChar(response$content),
                    sep = "\r\n"))
}
