# The remaining registered IMAP capabilities, swept from the IANA registry in
# 2.2.0: PARTIAL/CONTEXT (paged search), REPLACE, OBJECTID, UIDBATCHES,
# MULTISEARCH, UNAUTHENTICATE, LANGUAGE/I18NLEVEL, URLAUTH, CONVERT,
# ANNOTATE-EXPERIMENT-1, and APPENDLIMIT. Few servers deploy most of them, so
# the corresponding methods are documented as experimental: they follow the
# RFC grammars but could not be exercised against a live server.

#' Stop when a message exceeds the server's advertised APPENDLIMIT (RFC 7889)
#' @noRd
assert_within_appendlimit <- function(self, sizes, retries = 1) {
  caps <- toupper(get_server_capabilities(self, retries = retries))
  lim <- sub("^APPENDLIMIT=", "", grep("^APPENDLIMIT=[0-9]+$", caps, value = TRUE))
  if (length(lim) == 1) {
    lim <- as.numeric(lim)
    if (any(sizes > lim)) {
      stop(sprintf(paste0("The server limits APPEND to %s bytes per message ",
                          "(APPENDLIMIT, RFC 7889), and a message of %s bytes ",
                          "was given."),
                   format(lim, scientific = FALSE),
                   format(max(sizes), scientific = FALSE)), call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Parse the PARTIAL item of an ESEARCH response (RFC 9394 / RFC 5267)
#'
#' Reads \code{PARTIAL (m:n <set>)} preserving the server order (ranges may be
#' descending in a SORT result). Returns \code{integer(0)} when the item is
#' absent or \code{NIL}, with the requested range kept in the \code{"range"}
#' attribute.
#' @noRd
parse_esort_partial <- function(content_char) {
  m <- stringr::str_match(content_char,
                          "PARTIAL \\((-?[0-9]+:-?[0-9]+) (NIL|[0-9,:]+)\\)")
  if (is.na(m[1, 1])) return(integer(0))
  ids <- integer(0)
  if (!identical(m[1, 3], "NIL")) {
    for (tok in strsplit(m[1, 3], ",", fixed = TRUE)[[1]]) {
      if (grepl(":", tok, fixed = TRUE)) {
        b <- as.integer(strsplit(tok, ":", fixed = TRUE)[[1]])
        ids <- c(ids, seq.int(b[1], b[2]))
      } else {
        ids <- c(ids, as.integer(tok))
      }
    }
  }
  attr(ids, "range") <- m[1, 2]
  ids
}

#' Paged search: SEARCH RETURN (PARTIAL m:n) (RFC 9394 / RFC 5267) (INTERNAL HELPER)
#' @noRd
esearch_partial_int <- function(self, range, criteria, use_uid, retries) {
  assertthat::assert_that(is.character(range), length(range) == 1,
                          grepl("^-?[0-9]+:-?[0-9]+$", range),
                          msg='"range" must be a single "m:n" string, e.g. "1:100" (or "-1:-100" for the most recent results).')
  assertthat::assert_that(is.character(criteria), msg='"criteria" must be of type character.')
  assertthat::assert_that(!is.na(self$con_params$folder), msg='No folder previously selected.')
  check_args(use_uid = use_uid, retries = retries)
  caps <- toupper(get_server_capabilities(self, retries = retries))
  if (!("PARTIAL" %in% caps) && !("CONTEXT=SEARCH" %in% caps)) {
    stop(paste0('The IMAP server advertises neither the "PARTIAL" (RFC 9394) nor the ',
                '"CONTEXT=SEARCH" (RFC 5267) capability, one of which is required by the ',
                '"esearch_partial" command. This is a server limitation, not an error in ',
                'your call. Check what your server supports with `list_server_capabilities()`.'),
         call. = FALSE)
  }
  retries <- as.integer(retries)
  url <- self$con_params$url
  h <- self$con_handle
  prefix <- if (isTRUE(use_uid)) "UID SEARCH " else "SEARCH "
  customrequest <- paste0(prefix, "RETURN (PARTIAL ", range, ") ", criteria)
  response <- execute_complementary_operations(self, url, handle = h,
                                               customrequest, retries)
  out <- parse_esort_partial(paste(rawToChar(response$headers),
                                   rawToChar(response$content), sep = "\r\n"))
  rm(h)
  out
}

#' Paged sort: SORT RETURN (PARTIAL m:n) (CONTEXT=SORT, RFC 5267) (INTERNAL HELPER)
#' @noRd
esort_partial_int <- function(self, range, by, reverse, criteria, use_uid,
                              char_set, retries) {
  assertthat::assert_that(is.character(range), length(range) == 1,
                          grepl("^-?[0-9]+:-?[0-9]+$", range),
                          msg='"range" must be a single "m:n" string, e.g. "1:100".')
  assertthat::assert_that(is.character(by), msg='"by" must be a character vector of sort keys.')
  assertthat::assert_that(is.logical(reverse), msg='"reverse" must be a logical.')
  assertthat::assert_that(is.character(criteria), msg='"criteria" must be of type character.')
  assertthat::assert_that(is.character(char_set), msg='"char_set" must be of type character.')
  assertthat::assert_that(!is.na(self$con_params$folder), msg='No folder previously selected.')
  check_args(use_uid = use_uid, retries = retries)
  by <- toupper(by)
  valid_keys <- c("ARRIVAL", "CC", "DATE", "FROM", "SIZE", "SUBJECT", "TO",
                  "DISPLAYFROM", "DISPLAYTO")
  assertthat::assert_that(all(by %in% valid_keys),
                          msg = paste0('"by" must be a subset of: ',
                                       paste(valid_keys, collapse = ", "), '.'))
  assert_capability(self, "SORT", command = "esort_partial", rfc = "RFC 5256",
                    retries = retries)
  assert_capability(self, "CONTEXT=SORT", command = "esort_partial",
                    rfc = "RFC 5267", retries = retries)
  if (any(c("DISPLAYFROM", "DISPLAYTO") %in% by)) {
    assert_capability(self, "SORT=DISPLAY", command = "esort_partial (DISPLAY keys)",
                      rfc = "RFC 5957", retries = retries)
  }
  retries <- as.integer(retries)
  keys <- if (isTRUE(reverse)) paste0("REVERSE ", by) else by
  keys_str <- paste0("(", paste(keys, collapse = " "), ")")
  prefix <- if (isTRUE(use_uid)) "UID SORT " else "SORT "
  customrequest <- paste0(prefix, "RETURN (PARTIAL ", range, ") ", keys_str, " ",
                          char_set, " ", criteria)
  url <- self$con_params$url
  h <- self$con_handle
  tryCatch({
    curl::handle_setopt(handle = h, customrequest = customrequest)
  }, error = function(e) {
    stop("The connection handle is dead. Please, configure a new IMAP connection with configure_imap().")
  })
  execute_ordered_search(self = self, url = url, handle = h,
                         customrequest = customrequest,
                         parser = parse_esort_partial, retries = retries)
}

#' Replace a message in place: [UID] REPLACE (RFC 8508) (INTERNAL HELPER)
#' @noRd
replace_msg_int <- function(self, auth, msg_id, message, folder, flags, use_uid,
                            mute, compress, retries) {
  assertthat::assert_that(is.numeric(msg_id), length(msg_id) == 1,
                          msg='"msg_id" must be a single message id.')
  assertthat::assert_that(is.character(message) || is.raw(message),
                          msg='"message" must be a character string or a raw vector (a full RFC 822 message).')
  if (is.null(folder)) {
    assertthat::assert_that(!is.na(self$con_params$folder),
                            msg='No folder previously selected. Provide "folder" or select one first.')
    folder <- self$con_params$folder
  }
  check_args(use_uid = use_uid, mute = mute, retries = retries)
  assert_capability(self, "REPLACE", command = "replace_msg", rfc = "RFC 8508",
                    retries = retries)
  payload <- if (is.raw(message)) message else charToRaw(enc2utf8(paste(message, collapse = "\r\n")))
  assert_within_appendlimit(self, length(payload), retries)
  flags_str <- if (is.null(flags)) "" else
    paste0("(", paste(paste0("\\", sub("^\\\\", "", flags)), collapse = " "), ") ")
  sess <- raw_session_start(self, auth, compress = compress,
                            timeout_ms = max(30000, self$con_params$timeout_ms))
  on.exit(raw_session_close(sess), add = TRUE)
  raw_select(sess, folder)
  prefix <- if (isTRUE(use_uid)) "UID REPLACE " else "REPLACE "
  cmd <- paste0(prefix, format(msg_id, scientific = FALSE), " ",
                adjust_folder_name(folder), " ", flags_str, "{", length(payload), "}")
  r <- raw_command(sess, cmd, literals = list(payload),
                   timeout_ms = max(60000, self$con_params$timeout_ms))
  raw_ok_or_stop(r, "REPLACE")
  raw_sync_main(self, folder, retries)
  m <- stringr::str_match(paste(c(r$lines, r$tagged), collapse = "\n"),
                          "\\[APPENDUID\\s+(\\d+)\\s+([0-9,:]+)\\]")
  uid <- if (is.na(m[1, 3])) NA_integer_ else as.integer(m[1, 3])
  if (!mute) {
    cat(paste0("\n::mRpostman: message ", format(msg_id, scientific = FALSE),
               " replaced in \"", folder, "\".\n"))
  }
  invisible(uid)
}

#' Fetch the unique object identifiers of messages (OBJECTID, RFC 8474) (INTERNAL HELPER)
#' @noRd
fetch_objectid_int <- function(self, msg_id, use_uid, retries) {
  check_args(msg_id = msg_id, use_uid = use_uid, retries = retries)
  assertthat::assert_that(!is.na(self$con_params$folder), msg='No folder previously selected.')
  assert_capability(self, "OBJECTID", command = "fetch_objectid", rfc = "RFC 8474",
                    retries = retries)
  retries <- as.integer(retries)
  url <- self$con_params$url
  h <- self$con_handle
  ids <- paste(format(msg_id, scientific = FALSE, trim = TRUE), collapse = ",")
  prefix <- if (isTRUE(use_uid)) "UID FETCH " else "FETCH "
  customrequest <- paste0(prefix, ids, " (EMAILID THREADID)")
  response <- execute_complementary_operations(self, url, handle = h,
                                               customrequest, retries)
  txt <- paste(rawToChar(response$headers), rawToChar(response$content), sep = "\r\n")
  rm(h)
  parse_objectid(txt, use_uid = use_uid)
}

#' Parse EMAILID/THREADID items of a FETCH response (RFC 8474)
#' @noRd
parse_objectid <- function(txt, use_uid = FALSE) {
  m <- stringr::str_match_all(txt, "\\*\\s+(\\d+)\\s+FETCH\\s+\\(([^\r\n]*)\\)")[[1]]
  if (nrow(m) == 0) {
    return(data.frame(id = integer(0), emailid = character(0),
                      threadid = character(0), stringsAsFactors = FALSE))
  }
  uid <- stringr::str_match(m[, 3], "\\bUID\\s+(\\d+)")[, 2]
  id <- ifelse(isTRUE(use_uid) & !is.na(uid), as.integer(uid), as.integer(m[, 2]))
  emailid <- stringr::str_match(m[, 3], "EMAILID\\s+\\(([^)]+)\\)")[, 2]
  threadid <- stringr::str_match(m[, 3], "THREADID\\s+(?:\\(([^)]+)\\)|NIL)")[, 2]
  data.frame(id = id, emailid = emailid, threadid = threadid,
             stringsAsFactors = FALSE)
}

#' Partition the mailbox into UID batches (UIDBATCHES, RFC 10022) (INTERNAL HELPER)
#' @noRd
uid_batches_int <- function(self, batch_size, retries) {
  assertthat::assert_that(is.numeric(batch_size), length(batch_size) == 1,
                          batch_size >= 1,
                          msg='"batch_size" must be a single positive number.')
  assertthat::assert_that(!is.na(self$con_params$folder), msg='No folder previously selected.')
  check_args(retries = retries)
  assert_capability(self, "UIDBATCHES", command = "uid_batches", rfc = "RFC 10022",
                    retries = retries)
  retries <- as.integer(retries)
  url <- self$con_params$url
  h <- self$con_handle
  customrequest <- paste0("UIDBATCHES ", format(batch_size, scientific = FALSE, trim = TRUE))
  response <- execute_complementary_operations(self, url, handle = h,
                                               customrequest, retries)
  txt <- paste(rawToChar(response$headers), rawToChar(response$content), sep = "\r\n")
  rm(h)
  parse_uid_batches(txt)
}

#' Parse the UIDBATCHES untagged response (RFC 10022)
#' @noRd
parse_uid_batches <- function(txt) {
  m <- stringr::str_match(txt, "\\*\\s+UIDBATCHES\\s+\\(TAG\\s+\"[^\"]*\"\\)[ ]*([0-9:,]*)")
  empty <- data.frame(from = integer(0), to = integer(0), stringsAsFactors = FALSE)
  if (is.na(m[1, 2]) || !nzchar(m[1, 2])) return(empty)
  parts <- strsplit(m[1, 2], ",", fixed = TRUE)[[1]]
  b <- vapply(strsplit(parts, ":", fixed = TRUE),
              function(x) as.integer(x[1:2]), integer(2))
  data.frame(from = b[1, ], to = b[2, ], stringsAsFactors = FALSE)
}

#' Search several mailboxes in one command (MULTISEARCH, RFC 7377) (INTERNAL HELPER)
#' @noRd
esearch_multi_int <- function(self, auth, mailboxes, criteria, compress, retries) {
  assertthat::assert_that(is.character(mailboxes), length(mailboxes) >= 1,
                          msg='"mailboxes" must be "personal", "subscribed", "inboxes", "selected", or a vector of folder names.')
  assertthat::assert_that(is.character(criteria), msg='"criteria" must be of type character.')
  check_args(retries = retries)
  assert_capability(self, "MULTISEARCH", command = "esearch_multi", rfc = "RFC 7377",
                    retries = retries)
  scope <- if (length(mailboxes) == 1 &&
               tolower(mailboxes) %in% c("personal", "subscribed", "inboxes", "selected")) {
    toupper(mailboxes)
  } else {
    paste0("MAILBOXES ", paste(vapply(mailboxes, adjust_folder_name, ""), collapse = " "))
  }
  sess <- raw_session_start(self, auth, compress = compress,
                            timeout_ms = max(30000, self$con_params$timeout_ms))
  on.exit(raw_session_close(sess), add = TRUE)
  r <- raw_command(sess, paste0("ESEARCH IN (", scope, ") RETURN () ", criteria),
                   timeout_ms = max(60000, self$con_params$timeout_ms))
  raw_ok_or_stop(r, "ESEARCH IN (multimailbox search)")
  parse_esearch_multi(r$lines)
}

#' Parse the untagged ESEARCH responses of a multimailbox search (RFC 7377)
#' @noRd
parse_esearch_multi <- function(lines) {
  empty <- data.frame(mailbox = character(0), uidvalidity = integer(0),
                      uid = integer(0), stringsAsFactors = FALSE)
  if (length(lines) == 0) return(empty)
  m <- stringr::str_match(lines, paste0('^\\*\\s+ESEARCH\\s+\\(TAG\\s+"[^"]*"\\s+MAILBOX\\s+',
                                        '(?:"([^"]*)"|([^\\s)]+))\\s+UIDVALIDITY\\s+(\\d+)\\)(.*)$'))
  ok <- which(!is.na(m[, 1]))
  if (length(ok) == 0) return(empty)
  out <- lapply(ok, function(i) {
    mbx <- if (!is.na(m[i, 2])) m[i, 2] else m[i, 3]
    ids <- stringr::str_match(m[i, 5], "\\bALL\\s+([0-9,:]+)")[1, 2]
    ids <- if (is.na(ids)) integer(0) else expand_sequence_set(ids)
    if (length(ids) == 0) return(NULL)
    data.frame(mailbox = mbx, uidvalidity = as.integer(m[i, 4]), uid = ids,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, out)
  if (is.null(out)) empty else out
}

#' Return the connection to the not-authenticated state (UNAUTHENTICATE, RFC 8437) (INTERNAL HELPER)
#' @noRd
unauthenticate_int <- function(self, retries) {
  check_args(retries = retries)
  assert_capability(self, "UNAUTHENTICATE", command = "unauthenticate",
                    rfc = "RFC 8437", retries = retries)
  execute_simple_command(self, "UNAUTHENTICATE", as.integer(retries))
  # the shared libcurl connection is now unauthenticated and useless to
  # libcurl's request model: open a fresh one (which authenticates again) so
  # the connection object stays usable
  self$con_params$folder <- NA_character_
  self$server_capabilities <- NULL
  tryCatch({
    curl::handle_setopt(self$con_handle, fresh_connect = TRUE)
    noop_int(self, retries = 0)
    curl::handle_setopt(self$con_handle, fresh_connect = FALSE)
  }, error = function(e) NULL)
  invisible(TRUE)
}

#' Negotiate the language of server responses (LANGUAGE, RFC 5255) (INTERNAL HELPER)
#' @noRd
language_int <- function(self, language, retries) {
  check_args(retries = retries)
  assert_capability(self, "LANGUAGE", command = "language", rfc = "RFC 5255",
                    retries = retries)
  if (!is.null(language)) {
    assertthat::assert_that(is.character(language),
                            msg='"language" must be NULL or a character vector of RFC 4646 language tags.')
  }
  customrequest <- if (is.null(language)) "LANGUAGE" else
    paste0("LANGUAGE ", paste0('"', language, '"', collapse = " "))
  resp <- execute_simple_command(self, customrequest, as.integer(retries))
  m <- stringr::str_match(resp, "\\*\\s+LANGUAGE\\s+\\(([^)]*)\\)")
  if (is.na(m[1, 2])) character(0) else
    strsplit(trimws(gsub('"', "", m[1, 2])), "\\s+")[[1]]
}

#' Choose the collation comparator (COMPARATOR, I18NLEVEL=2, RFC 5255) (INTERNAL HELPER)
#' @noRd
comparator_int <- function(self, order, retries) {
  check_args(retries = retries)
  assert_capability(self, "I18NLEVEL=2", command = "comparator", rfc = "RFC 5255",
                    retries = retries)
  if (!is.null(order)) {
    assertthat::assert_that(is.character(order),
                            msg='"order" must be NULL or a character vector of comparator names (e.g. "i;basic").')
  }
  customrequest <- if (is.null(order)) "COMPARATOR" else
    paste0("COMPARATOR ", paste0('"', order, '"', collapse = " "))
  resp <- execute_simple_command(self, customrequest, as.integer(retries))
  m <- stringr::str_match(resp, "\\*\\s+COMPARATOR\\s+([^\r\n]*)")
  if (is.na(m[1, 2])) character(0) else gsub('"', "", trimws(m[1, 2]))
}

#' Generate an authorized IMAP URL (GENURLAUTH, URLAUTH, RFC 4467) (INTERNAL HELPER)
#' @noRd
genurlauth_int <- function(self, url, access, mechanism, expire, retries) {
  assertthat::assert_that(is.character(url) || inherits(url, "imap_url"),
                          length(url) == 1,
                          msg='"url" must be an imap_url() object or an IMAP URL string.')
  assertthat::assert_that(is.character(access), length(access) == 1,
                          msg='"access" must be a single string, e.g. "anonymous", "authuser", "submit+<user>", or "user+<user>".')
  check_args(retries = retries)
  assert_capability(self, "URLAUTH", command = "genurlauth", rfc = "RFC 4467",
                    retries = retries)
  url <- unclass(url)
  if (!grepl("^imap://", url)) {
    host <- sub("^imaps?://([^:/]+).*$", "\\1", self$con_params$url)
    url <- paste0("imap://", self$con_params$username, "@", host, url)
  }
  if (!grepl(";URLAUTH=", url, ignore.case = TRUE)) {
    url <- paste0(url,
                  if (!is.null(expire)) paste0(";EXPIRE=", expire) else "",
                  ";URLAUTH=", access)
  }
  resp <- execute_simple_command(self, paste0("GENURLAUTH \"", url, "\" ",
                                              toupper(mechanism)),
                                 as.integer(retries))
  m <- stringr::str_match_all(resp, "\\*\\s+GENURLAUTH\\s+\"([^\"]+)\"")[[1]]
  if (nrow(m) == 0) character(0) else m[, 2]
}

#' Fetch the content named by authorized IMAP URLs (URLFETCH, RFC 4467) (INTERNAL HELPER)
#' @noRd
urlfetch_int <- function(self, auth, urls, compress, retries) {
  assertthat::assert_that(is.character(urls), length(urls) >= 1,
                          msg='"urls" must be a character vector of URLAUTH-authorized IMAP URLs.')
  check_args(retries = retries)
  assert_capability(self, "URLAUTH", command = "urlfetch", rfc = "RFC 4467",
                    retries = retries)
  sess <- raw_session_start(self, auth, compress = compress,
                            timeout_ms = max(30000, self$con_params$timeout_ms))
  on.exit(raw_session_close(sess), add = TRUE)
  out <- vector("list", length(urls))
  for (i in seq_along(urls)) {
    r <- raw_command(sess, paste0("URLFETCH \"", urls[i], "\""),
                     timeout_ms = max(60000, self$con_params$timeout_ms))
    raw_ok_or_stop(r, "URLFETCH")
    out[[i]] <- if (length(r$literals)) r$literals[[1]] else raw(0)
  }
  names(out) <- urls
  out
}

#' Fetch a body part converted by the server (CONVERT, RFC 5259) (INTERNAL HELPER)
#' @noRd
fetch_convert_int <- function(self, auth, msg_id, mimetype, part, params,
                              use_uid, folder, compress, retries) {
  check_args(use_uid = use_uid, retries = retries)
  assertthat::assert_that(is.numeric(msg_id), length(msg_id) == 1,
                          msg='"msg_id" must be a single message id.')
  assertthat::assert_that(is.character(mimetype), length(mimetype) == 1,
                          msg='"mimetype" must be a single MIME type, e.g. "application/pdf".')
  assertthat::assert_that(is.character(part), length(part) == 1,
                          grepl("^[0-9.]+$", part),
                          msg='"part" must be a single section number, e.g. "1" or "2.1".')
  if (is.null(folder)) {
    assertthat::assert_that(!is.na(self$con_params$folder), msg='No folder previously selected.')
    folder <- self$con_params$folder
  }
  assert_capability(self, "CONVERT", command = "fetch_convert", rfc = "RFC 5259",
                    retries = retries)
  params_str <- if (is.null(params)) "" else
    paste0(" (", paste0('"', names(params), '" "', unlist(params), '"', collapse = " "), ")")
  sess <- raw_session_start(self, auth, compress = compress,
                            timeout_ms = max(30000, self$con_params$timeout_ms))
  on.exit(raw_session_close(sess), add = TRUE)
  raw_select(sess, folder)
  prefix <- if (isTRUE(use_uid)) "UID CONVERT " else "CONVERT "
  cmd <- paste0(prefix, format(msg_id, scientific = FALSE), " (\"", mimetype, "\"",
                params_str, ") BINARY[", part, "]")
  r <- raw_command(sess, cmd, timeout_ms = max(60000, self$con_params$timeout_ms))
  raw_ok_or_stop(r, "CONVERT")
  if (length(r$literals)) r$literals[[1]] else raw(0)
}

#' Fetch per-message annotations (ANNOTATE-EXPERIMENT-1, RFC 5257) (INTERNAL HELPER)
#' @noRd
fetch_annotation_int <- function(self, msg_id, entries, attributes, use_uid, retries) {
  check_args(msg_id = msg_id, use_uid = use_uid, retries = retries)
  assertthat::assert_that(is.character(entries), length(entries) >= 1,
                          msg='"entries" must be a character vector of annotation entries, e.g. "/comment" or "/*".')
  assertthat::assert_that(is.character(attributes), length(attributes) >= 1,
                          msg='"attributes" must be a character vector, e.g. "value", "value.priv", "size".')
  assertthat::assert_that(!is.na(self$con_params$folder), msg='No folder previously selected.')
  assert_capability(self, "ANNOTATE-EXPERIMENT-1", command = "fetch_annotation",
                    rfc = "RFC 5257", retries = retries)
  retries <- as.integer(retries)
  wrap <- function(x) if (length(x) == 1) paste0('"', x, '"') else
    paste0("(", paste0('"', x, '"', collapse = " "), ")")
  ids <- paste(format(msg_id, scientific = FALSE, trim = TRUE), collapse = ",")
  prefix <- if (isTRUE(use_uid)) "UID FETCH " else "FETCH "
  customrequest <- paste0(prefix, ids, " (ANNOTATION (", wrap(entries), " ",
                          wrap(attributes), "))")
  url <- self$con_params$url
  h <- self$con_handle
  response <- execute_complementary_operations(self, url, handle = h,
                                               customrequest, retries)
  txt <- paste(rawToChar(response$headers), rawToChar(response$content), sep = "\r\n")
  rm(h)
  parse_annotation(txt, use_uid = use_uid)
}

#' Parse the ANNOTATION items of a FETCH response (RFC 5257)
#' @noRd
parse_annotation <- function(txt, use_uid = FALSE) {
  empty <- data.frame(id = integer(0), entry = character(0),
                      attribute = character(0), value = character(0),
                      stringsAsFactors = FALSE)
  m <- stringr::str_match_all(txt,
    "\\*\\s+(\\d+)\\s+FETCH\\s+\\((?:UID\\s+(\\d+)\\s+)?ANNOTATION\\s+\\((.*)\\)\\)")[[1]]
  if (nrow(m) == 0) return(empty)
  out <- list()
  for (i in seq_len(nrow(m))) {
    id <- if (isTRUE(use_uid) && !is.na(m[i, 3])) as.integer(m[i, 3]) else as.integer(m[i, 2])
    eb <- stringr::str_match_all(m[i, 4],
      '"?(/[^\\s"]+)"?\\s+\\(((?:"[^"]*"|NIL|[^()])*)\\)')[[1]]
    for (j in seq_len(nrow(eb))) {
      av <- stringr::str_match_all(eb[j, 3],
        '"?([A-Za-z][A-Za-z.]*)"?\\s+(?:"([^"]*)"|(NIL)|(\\d+))')[[1]]
      if (nrow(av) == 0) next
      value <- ifelse(!is.na(av[, 4]), NA_character_,
                      ifelse(!is.na(av[, 5]), av[, 5], av[, 3]))
      out[[length(out) + 1]] <- data.frame(id = id, entry = eb[j, 2],
                                           attribute = av[, 2], value = value,
                                           stringsAsFactors = FALSE)
    }
  }
  if (length(out) == 0) empty else do.call(rbind, out)
}

#' Store a per-message annotation (ANNOTATE-EXPERIMENT-1, RFC 5257) (INTERNAL HELPER)
#' @noRd
store_annotation_int <- function(self, msg_id, entry, values, use_uid, mute, retries) {
  check_args(msg_id = msg_id, use_uid = use_uid, mute = mute, retries = retries)
  assertthat::assert_that(is.character(entry), length(entry) == 1,
                          msg='"entry" must be a single annotation entry, e.g. "/comment".')
  assertthat::assert_that(is.character(values) || all(is.na(values)),
                          length(values) >= 1, !is.null(names(values)),
                          all(names(values) %in% c("value.priv", "value.shared")),
                          msg='"values" must be a named vector with names "value.priv" and/or "value.shared" (use NA to delete an annotation).')
  assertthat::assert_that(!is.na(self$con_params$folder), msg='No folder previously selected.')
  assert_capability(self, "ANNOTATE-EXPERIMENT-1", command = "store_annotation",
                    rfc = "RFC 5257", retries = retries)
  retries <- as.integer(retries)
  vals_str <- paste(vapply(seq_along(values), function(i) {
    paste0('"', names(values)[i], '" ',
           if (is.na(values[i])) "NIL" else paste0('"', values[i], '"'))
  }, ""), collapse = " ")
  ids <- paste(format(msg_id, scientific = FALSE, trim = TRUE), collapse = ",")
  prefix <- if (isTRUE(use_uid)) "UID STORE " else "STORE "
  customrequest <- paste0(prefix, ids, ' ANNOTATION ("', entry, '" (', vals_str, "))")
  url <- self$con_params$url
  h <- self$con_handle
  execute_complementary_operations(self, url, handle = h, customrequest, retries)
  rm(h)
  if (!mute) {
    cat(paste0("\n::mRpostman: annotation \"", entry, "\" stored.\n"))
  }
  invisible(TRUE)
}
