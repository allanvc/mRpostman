# Rebuild a server reply as the single byte stream it was on the wire
# (INTERNAL ENGINE)
#
# libcurl hands the reply of a custom IMAP command back through two
# callbacks, the "header" one (response$headers) and the "body" one
# (response$content), and which bytes reach which callback depends on the
# libcurl version and on the shape of the request:
#
#   - up to 8.16.x: every response line, literal bytes included, goes to the
#     header callback; the body callback only receives the untagged lines
#     libcurl recognised. `headers` is the wire, `content` a subset of it.
#   - 8.17.0 and later (curl/curl#19246): an untagged line that ends in a
#     literal marker "{n}" is written to the body callback together with the n
#     bytes of the literal, which no longer reach the header callback at all;
#     what follows the literal (the rest of the line, ")" and the tagged line)
#     still arrives as headers.
#   - 8.19.0 and later (curl commit e788d9d2c, "imap: fix custom listing"): a
#     FETCH whose sequence set is a range or a list ("1:*", "1,2") takes the
#     pre-8.17 path again. Two upstream pull requests (curl/curl#22915 and
#     #22951) change that classification once more.
#
# On top of that, 8.17.0 and later leave the ")" and the tagged completion
# line of a literal reply unread whenever the literal arrived in the same
# read as its announcing line (the whole reply fitted one buffer), and those
# lines are then delivered at the head of the next command's reply on the
# same connection. A reply carrying more than one literal is cut after the
# first one, and the rest surfaces the same way, with the next command.
#
# wire_rebuild() puts the two buffers back in wire order, so that every
# parser of the package keeps seeing the one layout it was written for (the
# pre-8.17 one). wire_split() then separates the leftovers of an earlier
# reply from the reply of the command itself and says whether that reply is
# closed (ends with its own tagged line). curl_perform() uses both, and
# flushes the connection when a reply is not closed. Everything is done on
# raw vectors: a literal may hold bytes in any encoding, or none.

# @param headers,content The header- and body-callback bytes of the transfer.
# @return A list: `raw`, the rebuilt stream; `missing`, TRUE when a literal
#   was announced but its bytes are in neither buffer.
# @noRd
wire_rebuild <- function(headers, content) {
  H <- if (is.raw(headers)) headers else charToRaw(headers)
  C <- if (is.raw(content)) content else charToRaw(content)
  nH <- length(H)
  nC <- length(C)
  if (nC == 0) {
    # nothing to stitch in: the header buffer is the wire as it stands
    return(list(raw = H, missing = wire_scan(H)$missing))
  }
  chunks <- list()
  missing <- FALSE
  pos <- 1L
  cpos <- 1L
  lf <- which(H == as.raw(0x0a))

  while (pos <= nH) {
    eol <- lf[lf >= pos]
    eol <- if (length(eol) > 0) eol[1] else nH
    line <- H[pos:eol]
    chunks[[length(chunks) + 1L]] <- line
    pos <- eol + 1L

    n <- literal_marker(line)
    if (is.na(n)) next

    # the literal's bytes: in the body buffer (8.17 and later) or right here
    # in the header buffer (every earlier release, and listings since 8.19)
    from_content <- FALSE
    hit <- if (cpos <= nC) grepRaw(line, C, offset = cpos, fixed = TRUE) else integer(0)
    if (length(hit) > 0) {
      start <- hit[1] + length(line)
      if (nC - start + 1L >= n) {
        take <- TRUE
        if (n > 0 && start + 1L <= nC && C[start] == as.raw(0x2a) && C[start + 1L] == as.raw(0x20)) {
          # the bytes look like an untagged line: before 8.17 the body buffer
          # holds the untagged lines of a multi-message reply one after the
          # other, and this would be the next one, not the literal. It is the
          # literal only if the header buffer does not hold that line too.
          first <- first_line(C[start:min(nC, start + n - 1L)])
          take <- length(grepRaw(first, H, offset = pos, fixed = TRUE)) == 0
        }
        if (take) {
          if (n > 0) chunks[[length(chunks) + 1L]] <- C[start:(start + n - 1L)]
          cpos <- start + n
          from_content <- TRUE
        }
      }
    }
    if (!from_content && n > 0) {
      avail <- nH - pos + 1L
      if (avail >= n) {
        chunks[[length(chunks) + 1L]] <- H[pos:(pos + n - 1L)]
        pos <- pos + n
      } else {
        if (avail > 0) chunks[[length(chunks) + 1L]] <- H[pos:nH]
        pos <- nH + 1L
        missing <- TRUE
      }
    }
  }
  list(raw = if (length(chunks) > 0) do.call(c, chunks) else raw(0), missing = missing)
}

# The protocol lines of a wire stream, skipping the bytes of every literal.
# @return A list: `start`/`end` (byte positions of each line, CRLF included),
#   `tag` (the tag of a tagged status line, NA otherwise), `missing` (a
#   literal announced by the last line is cut short).
# @noRd
wire_scan <- function(x) {
  n_x <- length(x)
  lf <- which(x == as.raw(0x0a))
  start <- integer(0); end <- integer(0); tag <- character(0)
  missing <- FALSE
  pos <- 1L
  while (pos <= n_x) {
    eol <- lf[lf >= pos]
    eol <- if (length(eol) > 0) eol[1] else n_x
    line <- x[pos:eol]
    start <- c(start, pos); end <- c(end, eol); tag <- c(tag, tagged_line_tag(line))
    pos <- eol + 1L
    n <- literal_marker(line)
    if (!is.na(n) && n > 0) {
      if (n_x - pos + 1L < n) missing <- TRUE
      pos <- pos + n
    }
  }
  list(start = start, end = end, tag = tag, missing = missing)
}

# Separate the leftovers of earlier replies from the reply of the command
# whose tag is `tag`.
# @return A list: `reply` (raw), `stale` (bytes dropped from the head),
#   `closed` (the reply ends with its own tagged line), `missing`.
# @noRd
wire_split <- function(x, tag = NULL) {
  s <- wire_scan(x)
  k <- length(s$start)
  tagged <- which(!is.na(s$tag))
  stale_end <- 0L
  if (length(tagged) > 0) {
    foreign <- if (!is.null(tag) && nzchar(tag)) {
      tagged[s$tag[tagged] != tag]
    } else {
      tagged[tagged < k]
    }
    if (length(foreign) > 0) stale_end <- max(foreign)
  }
  cut <- if (stale_end > 0) s$end[stale_end] else 0L
  reply <- if (cut < length(x)) x[(cut + 1L):length(x)] else raw(0)
  closed <- k > stale_end && !is.na(s$tag[k]) &&
    (is.null(tag) || !nzchar(tag) || identical(s$tag[k], tag))
  list(reply = reply, stale = cut, closed = closed, missing = s$missing)
}

# Byte position where the tagged line of `tag` ends in a wire stream, or 0.
# @noRd
wire_tag_end <- function(x, tag) {
  s <- wire_scan(x)
  i <- which(!is.na(s$tag) & s$tag == tag)
  if (length(i) == 0) 0L else s$end[i[1]]
}

# The size announced by a literal marker "{n}" that ends a response line,
# outside any quoted string (the rule libcurl applies), or NA.
literal_marker <- function(line) {
  txt <- rawToChar(line[line != as.raw(0)])
  m <- regmatches(txt, regexpr("\\{[0-9]+\\}\r?\n?$", txt, useBytes = TRUE))
  if (length(m) == 0) return(NA_integer_)
  before <- substr(txt, 1L, nchar(txt, type = "bytes") - nchar(m, type = "bytes"))
  if (in_quoted_string(before)) return(NA_integer_)
  as.integer(gsub("[^0-9]", "", m))
}

# Is the position right after `txt` inside a double-quoted string?
in_quoted_string <- function(txt) {
  bytes <- charToRaw(txt)
  inq <- FALSE
  i <- 1L
  n <- length(bytes)
  while (i <= n) {
    b <- bytes[i]
    if (inq) {
      if (b == as.raw(0x5c) && i < n) { i <- i + 2L; next }  # backslash escape
      if (b == as.raw(0x22)) inq <- FALSE
    } else if (b == as.raw(0x22)) {
      inq <- TRUE
    }
    i <- i + 1L
  }
  inq
}

# The tag of a tagged status line ("A005 OK ..."), or NA for any other line.
tagged_line_tag <- function(line) {
  txt <- rawToChar(line[line != as.raw(0)])
  m <- regmatches(txt, regexpr("^[A-Za-z0-9]+(?= (OK|NO|BAD|PREAUTH|BYE)( |\r|\n|$))",
                               txt, perl = TRUE, useBytes = TRUE))
  if (length(m) == 0) NA_character_ else m
}

# The first CRLF-terminated line of a raw vector (the whole vector when it
# has no line end).
first_line <- function(x) {
  lf <- which(x == as.raw(0x0a))
  if (length(lf) == 0) x else x[seq_len(lf[1])]
}

# A wire stream as text, NUL bytes dropped.
wire_text <- function(x) rawToChar(x[x != as.raw(0)])
