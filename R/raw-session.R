# A minimal IMAP session spoken directly over the raw socket of
# R/imap-socket.R. It exists for the few operations libcurl's request model
# cannot express: IDLE (a parked connection receiving unsolicited responses)
# and literals ({n}, needed by MULTIAPPEND). Everything else keeps going
# through libcurl.

raw_session_open <- function(self, timeout_ms = 30000) {
  url <- self$con_params$url
  if (grepl("^imap://", url) && isTRUE(self$con_params$use_ssl)) {
    stop(paste0("The event connection needs an imaps:// URL for TLS ",
                "(STARTTLS is not available on the raw socket); use ",
                "url = \"imaps://...\" or use_ssl = FALSE."), call. = FALSE)
  }
  sess <- new.env(parent = emptyenv())
  sess$sock <- imap_socket_open(url, timeout_ms, verify_peer = TRUE)
  sess$buffer <- raw(0)
  sess$tag <- 0L
  sess$timeout_ms <- timeout_ms
  # libcurl reads the server greeting itself while connecting, so it is
  # usually not available any more; when it is, it must be an OK/PREAUTH
  greeting <- raw_readline(sess, timeout_ms = 1500)
  if (!is.null(greeting) && !grepl("^\\* (OK|PREAUTH)", greeting)) {
    imap_socket_close(sess$sock)
    stop("unexpected greeting from the server: ", greeting, call. = FALSE)
  }
  sess$caps <- if (is.null(greeting)) character(0) else toupper(raw_capability_tokens(greeting))
  if (length(sess$caps) == 0) {
    r <- raw_command(sess, "CAPABILITY")
    sess$caps <- toupper(raw_capability_tokens(paste(r$lines, collapse = "\n")))
  }
  sess
}

raw_capability_tokens <- function(x) {
  m <- stringr::str_match(x, "CAPABILITY\\s+([^\\]\r\n]*)")[1, 2]
  if (is.na(m)) return(character(0))
  strsplit(trimws(m), "\\s+")[[1]]
}

# next CRLF-terminated line (without the CRLF); NULL on timeout
raw_readline <- function(sess, timeout_ms = sess$timeout_ms) {
  deadline <- Sys.time() + timeout_ms / 1000
  repeat {
    nl <- which(sess$buffer == as.raw(0x0a))
    if (length(nl) > 0) {
      line <- sess$buffer[seq_len(nl[1])]
      sess$buffer <- sess$buffer[-seq_len(nl[1])]
      out <- sub("\r$", "", rawToChar(line[line != as.raw(0)]))
      if (isTRUE(getOption("mRpostman.raw_debug"))) cat("< ", out, "\n", sep = "", file = stderr())
      return(out)
    }
    remaining <- as.numeric(difftime(deadline, Sys.time(), units = "secs")) * 1000
    if (remaining <= 0) return(NULL)
    chunk <- imap_socket_recv(sess$sock, timeout_ms = as.integer(min(remaining, 30000)))
    if (isTRUE(attr(chunk, "closed"))) stop("the server closed the connection", call. = FALSE)
    if (length(chunk) > 0) sess$buffer <- c(sess$buffer, chunk)
  }
}

raw_next_tag <- function(sess) {
  sess$tag <- sess$tag + 1L
  sprintf("R%03d", sess$tag)
}

# Send a tagged command and collect the response. `literals` is a list of raw
# vectors sent, in order, each time the server answers with a "+" continuation
# (the command text must contain the matching "{n}" markers).
raw_command <- function(sess, command, literals = list(), timeout_ms = sess$timeout_ms) {
  tag <- raw_next_tag(sess)
  # the command is sent in pieces: the text up to (and including) each "{n}"
  # literal marker goes first; after the server's "+" the literal's bytes are
  # sent, followed by the text up to the next marker (or by CRLF at the end)
  marks <- gregexpr("\\{[0-9]+\\+?\\}", command)[[1]]
  if (marks[1] < 0) {
    pieces <- command
  } else {
    ends <- marks + attr(marks, "match.length") - 1L
    pieces <- substring(command, c(1L, ends + 1L), c(ends, nchar(command)))
    pieces <- pieces[c(rep(TRUE, length(ends)), nzchar(pieces[length(pieces)]))]
  }
  imap_socket_send(sess$sock, paste0(tag, " ", pieces[1], "\r\n"), sess$timeout_ms)
  lines <- character(0)
  lit_i <- 0L
  repeat {
    ln <- raw_readline(sess, timeout_ms)
    if (is.null(ln)) stop("timeout waiting for the server's reply to ", command, call. = FALSE)
    if (startsWith(ln, "+")) {
      lit_i <- lit_i + 1L
      if (lit_i > length(literals)) stop("unexpected continuation request: ", ln, call. = FALSE)
      imap_socket_send(sess$sock, literals[[lit_i]], sess$timeout_ms)
      tail_text <- if (lit_i + 1L <= length(pieces)) pieces[lit_i + 1L] else ""
      imap_socket_send(sess$sock, paste0(tail_text, "\r\n"), sess$timeout_ms)
      next
    }
    if (startsWith(ln, paste0(tag, " "))) {
      status <- sub(paste0("^", tag, " (\\S+).*$"), "\\1", ln)
      return(list(status = status, tagged = ln, lines = lines))
    }
    lines <- c(lines, ln)
  }
}

raw_ok_or_stop <- function(r, what) {
  if (!identical(r$status, "OK")) {
    stop("The server rejected ", what, ": ", sub("^\\S+ ", "", r$tagged), call. = FALSE)
  }
  invisible(r)
}

# AUTHENTICATE with the credentials kept privately by the connection object
raw_login <- function(sess, self, auth) {
  user <- self$con_params$username
  if (!is.null(auth$xoauth2_bearer)) {
    mech <- toupper(if (is.null(auth$oauth_mechanism)) "XOAUTH2" else auth$oauth_mechanism)
    ir <- if (mech == "OAUTHBEARER") {
      host <- sub("^imaps?://([^:/]+).*$", "\\1", self$con_params$url)
      paste0("n,a=", user, ",\001host=", host, "\001port=993\001auth=Bearer ", auth$xoauth2_bearer, "\001\001")
    } else {
      paste0("user=", user, "\001auth=Bearer ", auth$xoauth2_bearer, "\001\001")
    }
    ir_raw <- charToRaw(enc2utf8(ir))
  } else {
    mech <- "PLAIN"
    # SASL PLAIN: NUL authcid NUL password (NUL bytes cannot live in R strings)
    ir_raw <- c(as.raw(0), charToRaw(enc2utf8(user)), as.raw(0),
                charToRaw(enc2utf8(auth$password)))
  }
  ir64 <- base64enc::base64encode(ir_raw)
  if ("SASL-IR" %in% sess$caps) {
    r <- raw_command(sess, paste("AUTHENTICATE", mech, ir64))
  } else {
    r <- raw_command(sess, paste("AUTHENTICATE", mech), literals = list(charToRaw(ir64)))
  }
  raw_ok_or_stop(r, "the authentication")
  caps <- raw_capability_tokens(paste(c(r$lines, r$tagged), collapse = "\n"))
  if (length(caps)) sess$caps <- toupper(caps)
  invisible(TRUE)
}

raw_select <- function(sess, folder) {
  r <- raw_command(sess, paste0("SELECT ", adjust_folder_name(folder)))
  raw_ok_or_stop(r, paste0("SELECT ", folder))
  invisible(r)
}

# Parse the unsolicited responses received while idling into events
raw_idle_events <- function(lines) {
  if (length(lines) == 0) {
    return(data.frame(type = character(0), id = integer(0), detail = character(0),
                      stringsAsFactors = FALSE))
  }
  m <- stringr::str_match(lines, "^\\*\\s+(\\d+)\\s+(EXISTS|RECENT|EXPUNGE|FETCH)\\b\\s*(.*)$")
  ok <- !is.na(m[, 1])
  data.frame(type = m[ok, 3], id = as.integer(m[ok, 2]), detail = m[ok, 4],
             stringsAsFactors = FALSE)
}

# IDLE loop: returns the events received until `timeout` seconds elapse or
# `callback` returns FALSE. The IDLE command is renewed every `renew` seconds
# (servers may drop connections idling for more than ~30 minutes).
raw_idle <- function(sess, timeout = 300, callback = NULL, renew = 25 * 60) {
  if (!("IDLE" %in% sess$caps)) {
    stop('The IMAP server does not advertise the "IDLE" capability (RFC 2177).', call. = FALSE)
  }
  deadline <- Sys.time() + timeout
  events <- raw_idle_events(character(0))
  keep_going <- TRUE
  while (keep_going && Sys.time() < deadline) {
    tag <- raw_next_tag(sess)
    imap_socket_send(sess$sock, paste0(tag, " IDLE\r\n"), sess$timeout_ms)
    cont <- raw_readline(sess)
    if (is.null(cont) || !startsWith(cont, "+")) {
      stop("the server did not accept IDLE: ", if (is.null(cont)) "(timeout)" else cont, call. = FALSE)
    }
    idle_until <- min(deadline, Sys.time() + renew)
    while (keep_going && Sys.time() < idle_until) {
      wait_ms <- max(100, as.numeric(difftime(idle_until, Sys.time(), units = "secs")) * 1000)
      ln <- raw_readline(sess, timeout_ms = as.integer(min(wait_ms, 30000)))
      if (is.null(ln)) next
      ev <- raw_idle_events(ln)
      if (nrow(ev) > 0) {
        events <- rbind(events, ev)
        if (!is.null(callback)) {
          res <- callback(ev)
          if (identical(res, FALSE)) keep_going <- FALSE
        }
      }
    }
    imap_socket_send(sess$sock, "DONE\r\n", sess$timeout_ms)
    repeat {
      ln <- raw_readline(sess)
      if (is.null(ln)) stop("timeout waiting for the end of IDLE", call. = FALSE)
      ev <- raw_idle_events(ln)
      if (nrow(ev) > 0) events <- rbind(events, ev)
      if (startsWith(ln, paste0(tag, " "))) break
    }
  }
  rownames(events) <- NULL
  events
}

raw_session_close <- function(sess) {
  if (!is.null(sess) && imap_socket_is_open(sess$sock)) {
    try(raw_command(sess, "LOGOUT", timeout_ms = 5000), silent = TRUE)
    imap_socket_close(sess$sock)
  }
  invisible(TRUE)
}
