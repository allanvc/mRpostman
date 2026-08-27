# The remaining raw-socket extensions: BINARY (RFC 3516), CATENATE (RFC 4469),
# and NOTIFY (RFC 5465). COMPRESS=DEFLATE (RFC 4978) is a session option of
# every raw operation (see raw_session_start()).

#' Fetch message parts decoded by the server (INTERNAL HELPER)
#'
#' Issues \code{FETCH <id> (BINARY.PEEK[<part>])} on the raw session: the
#' server reverses the transfer encoding and answers with a \code{~\{n\}}
#' literal8 carrying the bytes. Requires \code{BINARY}.
#' @noRd
fetch_binary_int <- function(self, auth, msg_id, part, use_uid, folder, compress, retries) {
  check_args(msg_id = msg_id, use_uid = use_uid, retries = retries)
  assertthat::assert_that(is.character(part), length(part) == 1, grepl("^[0-9.]+$", part),
                          msg='"part" must be a single section number, e.g. "1" or "2.1".')
  if (is.null(folder)) {
    assertthat::assert_that(!is.na(self$con_params$folder), msg='No folder previously selected.')
    folder <- self$con_params$folder
  }
  assert_capability(self, "BINARY", command = "fetch_binary", rfc = "RFC 3516", retries = retries)
  sess <- raw_session_start(self, auth, compress = compress,
                            timeout_ms = max(30000, self$con_params$timeout_ms))
  on.exit(raw_session_close(sess), add = TRUE)
  raw_select(sess, folder)
  prefix <- if (isTRUE(use_uid)) "UID FETCH " else "FETCH "
  out <- vector("list", length(msg_id))
  for (i in seq_along(msg_id)) {
    r <- raw_command(sess, paste0(prefix, msg_id[i], " (BINARY.PEEK[", part, "])"),
                     timeout_ms = max(60000, self$con_params$timeout_ms))
    raw_ok_or_stop(r, paste0("FETCH BINARY[", part, "] of message ", msg_id[i]))
    out[[i]] <- if (length(r$literals)) r$literals[[1]] else raw(0)
  }
  names(out) <- paste0("binary", if (isTRUE(use_uid)) "UID" else "", msg_id)
  out
}

#' An IMAP URL naming a message, or a part of it, for CATENATE (RFC 5092)
#'
#' Builds the relative URL that \code{ImapCon$append_catenate()} uses to refer
#' to a message stored on the server: \code{/<folder>;UID=<uid>} with an
#' optional \code{/;SECTION=<section>} (e.g. \code{"HEADER"}, \code{"TEXT"},
#' or a MIME part number such as \code{"2"}).
#' @param folder A \code{character} string with the folder name.
#' @param uid The message UID.
#' @param section \code{NULL} (the whole message) or a section specifier.
#' @return An object of class \code{imap_url}.
#' @examples
#' imap_url("INBOX", uid = 12)
#' imap_url("INBOX", uid = 12, section = "HEADER")
#' @export
imap_url <- function(folder, uid, section = NULL) {
  assertthat::assert_that(is.character(folder), length(folder) == 1,
                          is.numeric(uid), length(uid) == 1,
                          msg='"folder" must be a string and "uid" a single number.')
  enc <- utils::URLencode(imap_utf7_encode(folder), reserved = TRUE)
  # RFC 5092: /<mailbox>/;UID=<uid>[/;SECTION=<section>]
  url <- paste0("/", enc, "/;UID=", format(uid, scientific = FALSE))
  if (!is.null(section)) url <- paste0(url, "/;SECTION=", section)
  structure(url, class = "imap_url")
}

#' Append a message assembled from server-side parts and text (INTERNAL HELPER)
#'
#' Issues \code{APPEND <folder> [flags] CATENATE (URL "..." TEXT {n} ...)}
#' (RFC 4469): \code{imap_url} parts are copied by the server from messages
#' it already stores, and character/raw parts are sent as literals.
#' @noRd
append_catenate_int <- function(self, auth, folder, parts, flags, compress, retries) {
  assertthat::assert_that(is.list(parts), length(parts) >= 1,
                          msg='"parts" must be a non-empty list of imap_url() objects, character strings, or raw vectors.')
  if (is.null(folder)) {
    assertthat::assert_that(!is.na(self$con_params$folder), msg='No folder previously selected.')
    folder <- self$con_params$folder
  }
  check_args(retries = retries)
  assert_capability(self, "CATENATE", command = "append_catenate", rfc = "RFC 4469",
                    retries = retries)
  flags_str <- if (is.null(flags)) "" else
    paste0("(", paste(paste0("\\", sub("^\\\\", "", flags)), collapse = " "), ") ")
  tokens <- character(0); literals <- list()
  for (p in parts) {
    if (inherits(p, "imap_url")) {
      tokens <- c(tokens, paste0("URL ", imap_quote(unclass(p))))
    } else if (is.raw(p)) {
      tokens <- c(tokens, paste0("TEXT {", length(p), "}")); literals <- c(literals, list(p))
    } else if (is.character(p)) {
      b <- charToRaw(enc2utf8(paste(p, collapse = "\r\n")))
      tokens <- c(tokens, paste0("TEXT {", length(b), "}")); literals <- c(literals, list(b))
    } else {
      stop("each part must be an imap_url() object, a character string, or a raw vector.", call. = FALSE)
    }
  }
  sess <- raw_session_start(self, auth, compress = compress,
                            timeout_ms = max(30000, self$con_params$timeout_ms))
  on.exit(raw_session_close(sess), add = TRUE)
  cmd <- paste0("APPEND ", adjust_folder_name(folder), " ", flags_str, "CATENATE (",
                paste(tokens, collapse = " "), ")")
  r <- raw_command(sess, cmd, literals = literals, timeout_ms = max(60000, self$con_params$timeout_ms))
  raw_ok_or_stop(r, "CATENATE")
  raw_sync_main(self, folder, retries)
  m <- stringr::str_match(r$tagged, "\\[APPENDUID\\s+(\\d+)\\s+(\\d+)\\]")
  if (is.na(m[1, 3])) NA_integer_ else as.integer(m[1, 3])
}

#' Receive server notifications for several mailboxes (INTERNAL HELPER)
#'
#' Issues \code{NOTIFY SET STATUS (<filter> (<events>)) ...} (RFC 5465) on the
#' raw session and collects the unsolicited responses (\code{EXISTS},
#' \code{EXPUNGE}, \code{FETCH} for the selected folder; \code{STATUS} for the
#' others; \code{LIST} for mailbox changes) until \code{timeout} seconds
#' elapse or \code{callback} returns \code{FALSE}; then \code{NOTIFY NONE}.
#' @noRd
notify_int <- function(self, auth, mailboxes, events, timeout, callback, compress, retries) {
  assertthat::assert_that(is.numeric(timeout), length(timeout) == 1, timeout > 0,
                          msg='"timeout" must be a positive number of seconds.')
  assertthat::assert_that(is.null(callback) || is.function(callback),
                          msg='"callback" must be NULL or a function of one argument.')
  assertthat::assert_that(is.character(mailboxes), length(mailboxes) >= 1,
                          msg='"mailboxes" must be "selected", "personal", "subscribed", "inboxes", or folder names.')
  valid_events <- c("MessageNew", "MessageExpunge", "FlagChange", "AnnotationChange",
                    "MailboxName", "SubscriptionChange")
  assertthat::assert_that(is.character(events), all(events %in% valid_events),
                          msg=paste0('"events" must be a subset of: ', paste(valid_events, collapse = ", "), "."))
  check_args(retries = retries)
  assert_capability(self, "NOTIFY", command = "notify", rfc = "RFC 5465", retries = retries)
  # MessageNew on the selected mailbox requires MessageExpunge as well (RFC 5465, 5)
  if ("MessageNew" %in% events && !("MessageExpunge" %in% events)) events <- c(events, "MessageExpunge")
  ev_str <- paste0("(", paste(events, collapse = " "), ")")
  filters <- tolower(mailboxes)
  groups <- character(0)
  special <- filters %in% c("selected", "personal", "subscribed", "inboxes")
  for (f in filters[special]) groups <- c(groups, paste0("(", toupper(f), " ", ev_str, ")"))
  named <- mailboxes[!special]
  if (length(named)) {
    groups <- c(groups, paste0("(MAILBOXES (", paste(vapply(named, adjust_folder_name, ""), collapse = " "), ") ", ev_str, ")"))
  }
  sess <- raw_session_start(self, auth, compress = compress,
                            timeout_ms = max(30000, self$con_params$timeout_ms))
  on.exit(raw_session_close(sess), add = TRUE)
  if ("selected" %in% filters) {
    assertthat::assert_that(!is.na(self$con_params$folder),
                            msg='"selected" requires a selected folder.')
    raw_select(sess, self$con_params$folder)
  }
  r <- raw_command(sess, paste("NOTIFY SET STATUS", paste(groups, collapse = " ")))
  raw_ok_or_stop(r, "NOTIFY SET")
  events_df <- raw_idle_events(r$lines)   # the initial STATUS of each mailbox
  deadline <- Sys.time() + timeout
  keep_going <- TRUE
  while (keep_going && Sys.time() < deadline) {
    wait_ms <- max(100, as.numeric(difftime(deadline, Sys.time(), units = "secs")) * 1000)
    ln <- raw_readline(sess, timeout_ms = as.integer(min(wait_ms, 30000)))
    if (is.null(ln)) next
    ev <- raw_idle_events(ln)
    if (nrow(ev) > 0) {
      events_df <- rbind(events_df, ev)
      if (!is.null(callback) && identical(callback(ev), FALSE)) keep_going <- FALSE
    }
  }
  try(raw_command(sess, "NOTIFY NONE", timeout_ms = 5000), silent = TRUE)
  rownames(events_df) <- NULL
  events_df
}
