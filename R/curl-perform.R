# Every libcurl transfer of a connection object runs in that object's own
# multi pool. curl_fetch_memory() performs on the package-wide pool shared by
# every handle of the R session, and libcurl's connection cache lives in the
# pool: a plaintext connection left there by another handle (one created with
# use_ssl = FALSE, or before a reset_use_ssl(TRUE)) matches a later handle
# to the same server and would be reused as is, since libcurl does not compare
# the requested TLS level when recycling a connection. With a private pool
# the only connections a handle can reuse are the ones it opened itself,
# under its own use_ssl setting.
#
# One difference from curl_fetch_memory(): in curl <= 7.1.0 the multi
# interface does not clear the handle's response-header buffer between
# transfers, so the `headers` of a result also carry the response lines of
# every earlier command on that handle. Whether the installed curl behaves
# that way is probed once per session (multi_headers_accumulate()); when it
# does, the bytes already in the buffer before the transfer (visible through
# handle_data()) are dropped from the result.
#
# Dropping them is not enough, though: the buffer itself keeps growing for the
# life of the handle, and every transfer copies the whole of it out of libcurl
# (handle_data(), the result) before the slice is taken, so a long run of
# commands on one connection (a fetch loop, ingest_maildir()) gets slower with
# every command, quadratically overall. Once the buffer passes
# handle_buffer_limit() bytes, renew_handle() resets the handle and applies
# its options again, which empties the buffer without touching the pool: the
# TCP connection cached there, with its login and selected folder, is reused
# by the next command exactly as before.
#
# Finally, the reply is handed back in one layout whatever the libcurl
# version: `headers` is the wire stream of the command's reply (literals in
# place, leftovers of earlier replies removed) and `complete` says whether
# the reply is whole. See wire-response.R.
curl_perform <- function(url, handle, self) {
  # the caller sets the command (customrequest) on the handle right before
  # this call, every time: a flush below may leave a NOOP on it
  tag_before <- debug_tag(self)
  out <- perform_transfer(url, handle, self)
  tag <- debug_tag(self)
  if (identical(tag, tag_before)) tag <- NULL  # no command went out (unknown)

  # one layout for every libcurl version: the wire, leftovers dropped (see
  # wire-response.R)
  built <- wire_rebuild(out$headers, out$content)
  parts <- wire_split(built$raw, tag)
  reply <- parts$reply
  closed <- parts$closed
  missing <- parts$missing

  # libcurl 8.17 and later may end the transfer before reading the ")" and
  # tagged line that close a literal reply (and the other literals of a
  # multi-literal one); they come with the next command. When the reply is
  # not closed, the connection is flushed with NOOPs until the tagged line
  # of the command arrives, and everything read up to it is the rest of the
  # reply. Each flush drains at least one literal, so the loop is short.
  flushes <- 0L
  reconnected <- FALSE
  while (!closed && !is.null(tag) && flushes < 8L) {
    flushes <- flushes + 1L
    epoch <- debug_epoch(self)
    curl::handle_setopt(handle, customrequest = "NOOP")
    more <- tryCatch(perform_transfer(url, handle, self), error = function(e) NULL)
    if (is.null(more)) break
    if (!identical(debug_epoch(self), epoch)) {
      # libcurl had closed the connection after the transfer (8.17 and 8.18
      # do so for some literal replies): the NOOP went out on a new one, and
      # nothing more of the reply can arrive. What was delivered is kept.
      reconnected <- TRUE
      break
    }
    more_raw <- wire_rebuild(more$headers, more$content)$raw
    cut <- wire_tag_end(more_raw, tag)
    if (cut > 0) {
      reply <- c(reply, more_raw[seq_len(cut)])
      closed <- TRUE
    } else {
      reply <- c(reply, more_raw)
    }
    missing <- wire_scan(reply)$missing
  }
  if (flushes > 0) {
    q <- self$con_debug
    if (!is.null(q)) q$flushes <- (if (is.null(q$flushes)) 0L else q$flushes) + flushes
  }

  out$headers <- reply
  # whole: every announced literal is there and the reply ended with its
  # tagged line, or could not go on at all (connection replaced)
  out$complete <- !missing && (closed || reconnected)
  out
}

# The tag of the command most recently sent on the connection, as seen by the
# debug callback (NULL when unknown).
debug_tag <- function(self) {
  q <- self$con_debug
  if (is.null(q) || is.null(q$tag) || !nzchar(q$tag)) NULL else q$tag
}

# The connection epoch counted by the debug callback (one more per
# connection libcurl opened), or 0 when unknown.
debug_epoch <- function(self) {
  q <- self$con_debug
  if (is.null(q) || is.null(q$epoch)) 0L else q$epoch
}

# One libcurl transfer on the connection's pool, headers of earlier transfers
# removed (see above). Nothing of the reply is interpreted here.
perform_transfer <- function(url, handle, self) {
  pool <- con_pool(self)
  curl::handle_setopt(handle, url = url)
  seen <- if (multi_headers_accumulate()) length(curl::handle_data(handle)$headers) else 0L
  out <- NULL
  err <- NULL
  finished <- FALSE
  # an interrupt inside multi_run() would leave the handle queued (and locked)
  on.exit(if (!finished) try(curl::multi_cancel(handle), silent = TRUE), add = TRUE)
  curl::multi_add(handle, done = function(res) out <<- res,
                  fail = function(msg) err <<- msg, pool = pool)
  curl::multi_run(pool = pool)
  finished <- TRUE
  if (!is.null(err)) stop(err, call. = FALSE)
  if (is.null(out)) stop("The transfer did not complete.", call. = FALSE)
  if (seen > 0L && length(out$headers) > seen) out$headers <- out$headers[-seq_len(seen)]
  if (seen > 0L && seen + length(out$headers) > handle_buffer_limit()) {
    renew_handle(self, handle)
  }
  out
}

# Bytes of accumulated response headers a handle may hold before it is
# renewed. Measured on the Docker sandbox (200-message fetch_text() rounds on
# one connection): with no limit the rounds grow 0.55 -> 3.0 s over six
# rounds; at 1 MiB they still swing between 0.5 and 1.0 s (every transfer
# copies up to 1 MiB); at 64 kB they hold at the 0.34 s a fresh connection
# per round gives, with a renewal every ~40 transfers; 16 kB gains nothing
# more. The option exists for diagnosis, not tuning.
handle_buffer_limit <- function() {
  lim <- getOption("mRpostman.handle_buffer_limit", 65536)
  if (!is.numeric(lim) || length(lim) != 1 || is.na(lim)) 65536 else lim
}

# Reset the handle in place and configure it again from the record kept on
# the connection object (con_opts: the libcurl options and the debug
# callback). Nothing happens when the record is missing (a connection built
# outside configure_imap(), or disconnected) or the handle is busy.
renew_handle <- function(self, handle) {
  opts <- self$con_opts
  if (is.null(opts) || is.null(opts$params)) return(invisible(FALSE))
  ok <- tryCatch({
    curl::handle_reset(handle)
    do.call(curl::handle_setopt, c(handle, opts$params))
    curl::handle_setopt(handle, verbose = TRUE, debugfunction = opts$debugfunction)
    TRUE
  }, error = function(e) FALSE)
  if (ok) opts$renewals <- opts$renewals + 1L
  invisible(ok)
}

# Does multi_add() keep the response headers of previous transfers on the
# handle? Decided once per session with two transfers of a local file:// URL
# (no network): the second result has more header bytes than the first when
# the buffer accumulates.
multi_headers_accumulate <- function() {
  if (!is.null(curl_quirks$accumulate)) return(curl_quirks$accumulate)
  tf <- tempfile("mrp-probe-")
  on.exit(unlink(tf), add = TRUE)
  writeLines("x", tf)
  url <- paste0("file://", if (.Platform$OS.type == "windows") "/" else "",
                normalizePath(tf, winslash = "/"))
  h <- curl::new_handle(url = url)
  pool <- curl::new_pool()
  n <- integer(0)
  for (i in 1:2) {
    curl::multi_add(h, done = function(res) n <<- c(n, length(res$headers)), pool = pool)
    curl::multi_run(pool = pool)
  }
  # an inconclusive probe (no headers at all) is treated as accumulating,
  # the behavior of every release so far
  curl_quirks$accumulate <- !(length(n) == 2 && n[2] == n[1] && n[1] > 0)
  curl_quirks$accumulate
}
curl_quirks <- new.env(parent = emptyenv())

# the pool is created lazily and dropped whenever the TLS requirement changes
# or the object disconnects, so that no cached connection outlives its setting
con_pool <- function(self) {
  if (is.null(self$con_pool)) self$con_pool <- curl::new_pool()
  self$con_pool
}
