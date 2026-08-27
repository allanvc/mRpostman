#' Wait for mailbox events on a dedicated connection (INTERNAL HELPER)
#'
#' Opens a second connection to the server on the raw socket layer, logs in
#' with the credentials kept by the connection object, selects the folder,
#' and issues \code{IDLE} (RFC 2177). The main libcurl connection is left
#' free, so the caller can fetch what the events announce.
#' @noRd
idle_int <- function(self, auth, timeout, callback, folder, renew, compress = FALSE) {
  assertthat::assert_that(is.numeric(timeout), length(timeout) == 1, timeout > 0,
                          msg='"timeout" must be a positive number of seconds.')
  assertthat::assert_that(is.null(callback) || is.function(callback),
                          msg='"callback" must be NULL or a function of one argument.')
  if (is.null(folder)) {
    assertthat::assert_that(!is.na(self$con_params$folder),
                            msg='No folder previously selected. Provide "folder" or select one first.')
    folder <- self$con_params$folder
  }
  assertthat::assert_that(is.character(folder), length(folder) == 1,
                          msg='"folder" must be a single character string.')
  assert_capability(self, "IDLE", command = "idle", rfc = "RFC 2177")

  sess <- raw_session_start(self, auth, compress = compress,
                            timeout_ms = max(30000, self$con_params$timeout_ms))
  on.exit(raw_session_close(sess), add = TRUE)
  raw_select(sess, folder)
  events <- raw_idle(sess, timeout = timeout, callback = callback, renew = renew)
  # the main connection learns about new messages only with its next command:
  # a NOOP brings its view of the folder up to date before the caller fetches
  if (!is.null(self$con_handle) && !is.na(self$con_params$folder)) {
    tryCatch(noop_int(self, retries = 0), error = function(e) NULL)
  }
  events
}

#' Append several messages in one command (MULTIAPPEND, RFC 3502) (INTERNAL HELPER)
#'
#' Uses the raw socket layer, since the command carries one literal per
#' message. Servers without \code{MULTIAPPEND} are served by one
#' \code{append_msg()} per message instead.
#' @noRd
append_msgs_int <- function(self, auth, messages, folder, flags, mute, retries,
                            compress = FALSE) {
  assertthat::assert_that(is.list(messages) || is.character(messages),
                          length(messages) >= 1,
                          msg='"messages" must be a character vector or a list of messages (character or raw).')
  if (is.character(messages)) messages <- as.list(messages)
  if (is.null(folder)) {
    assertthat::assert_that(!is.na(self$con_params$folder),
                            msg='No folder previously selected. Provide "folder" or select one first.')
    folder <- self$con_params$folder
  }
  check_args(mute = mute, retries = retries)
  caps <- toupper(get_server_capabilities(self, retries = retries))

  as_bytes <- function(m) {
    if (is.raw(m)) m else charToRaw(enc2utf8(paste(m, collapse = "\r\n")))
  }
  flags_str <- if (is.null(flags)) "" else
    paste0("(", paste(paste0("\\", sub("^\\\\", "", flags)), collapse = " "), ") ")

  if (!("MULTIAPPEND" %in% caps) || is.null(auth)) {
    # fall back to one APPEND per message through libcurl
    uids <- vapply(messages, function(m) {
      as.integer(append_int(self, m, folder, flags, mute = TRUE, retries = retries))
    }, integer(1))
    if (!mute) cat(paste0("\n::mRpostman: ", length(uids), " message(s) appended to \"", folder, "\".\n"))
    return(invisible(uids))
  }

  payloads <- lapply(messages, as_bytes)
  assert_within_appendlimit(self, vapply(payloads, length, integer(1)), retries)
  sess <- raw_session_start(self, auth, compress = compress,
                            timeout_ms = max(30000, self$con_params$timeout_ms))
  on.exit(raw_session_close(sess), add = TRUE)
  cmd <- paste0("APPEND ", adjust_folder_name(folder), " ",
                paste0(flags_str, "{", vapply(payloads, length, integer(1)), "}", collapse = " "))
  r <- raw_command(sess, cmd, literals = payloads,
                   timeout_ms = max(60000, self$con_params$timeout_ms))
  raw_ok_or_stop(r, "MULTIAPPEND")
  raw_sync_main(self, folder, retries)
  m <- stringr::str_match(r$tagged, "\\[APPENDUID\\s+(\\d+)\\s+([0-9,:]+)\\]")
  uids <- if (is.na(m[1, 3])) rep(NA_integer_, length(payloads)) else expand_sequence_set(m[1, 3])
  if (!mute) cat(paste0("\n::mRpostman: ", length(payloads), " message(s) appended to \"", folder, "\" in one MULTIAPPEND.\n"))
  invisible(uids)
}
