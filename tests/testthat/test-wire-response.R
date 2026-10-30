# The two callback buffers libcurl fills for a custom IMAP command, as
# captured on the Docker sandbox (Dovecot) with three libcurl builds:
# 8.14.1 (the layout the package was written for), 8.18.0 and 8.23.0
# (GitHub issue #18). wire_rebuild() must give the same stream for all.

old_text <- paste0("* 1 FETCH (UID 1 BODY[TEXT] {121}\r\n",
                   "This supersedes the previous version sent last week.\r\n\r\n",
                   "Let me know if you need any further details before the meeting.\r\n",
                   ")\r\nA004 OK Fetch completed (0.001 + 0.000 secs).\r\n")

test_that("wire_rebuild() keeps the pre-8.17 layout as it is", {
  # 8.14.1: the header buffer is the wire; the body buffer repeats the line
  w <- wire_rebuild(charToRaw(old_text), charToRaw("* 1 FETCH (UID 1 BODY[TEXT] {121}\r\n"))
  expect_identical(wire_text(w$raw), old_text)
  expect_false(w$missing)
  # 8.23.0 listing form: the body buffer is empty
  w <- wire_rebuild(charToRaw(old_text), raw(0))
  expect_identical(wire_text(w$raw), old_text)
})

test_that("wire_rebuild() puts a literal delivered as body back in place", {
  # 8.18.0 / 8.23.0: the announcing line and the literal come as body; the
  # header buffer holds only the announcing line (")" and the tagged line
  # come with the next command)
  h <- "* 1 FETCH (UID 1 BODY[TEXT] {121}\r\n"
  b <- paste0(h, "This supersedes the previous version sent last week.\r\n\r\n",
              "Let me know if you need any further details before the meeting.\r\n")
  w <- wire_rebuild(charToRaw(h), charToRaw(b))
  expect_identical(wire_text(w$raw), b)
  expect_false(w$missing)
  # once the tagged line has been read as well (a literal that did not fit
  # one read: 8.18.0 on a 158 kB part), the stream is the old layout again
  h2 <- "* 7 FETCH (UID 7 BODY[TEXT] {10}\r\n)\r\nA012 OK Fetch completed (0.001 + 0.000 secs).\r\n"
  b2 <- "* 7 FETCH (UID 7 BODY[TEXT] {10}\r\nxxxxxxxxxx"
  w <- wire_rebuild(charToRaw(h2), charToRaw(b2))
  expect_identical(wire_text(w$raw),
                   "* 7 FETCH (UID 7 BODY[TEXT] {10}\r\nxxxxxxxxxx)\r\nA012 OK Fetch completed (0.001 + 0.000 secs).\r\n")
})

test_that("wire_rebuild() handles a literal in the middle of a line (ENVELOPE)", {
  # Dovecot answers an 8-bit subject with a literal inside ENVELOPE
  rest <- " ((NIL NIL \"a\" \"b.c\")) ((NIL NIL \"a\" \"b.c\")) ((NIL NIL \"a\" \"b.c\")) NIL NIL NIL NIL NIL))\r\nA008 OK Fetch completed (0.001 + 0.000 secs).\r\n"
  subj <- "Relatório de vendas \"Q3\""
  lit <- charToRaw(enc2utf8(subj))
  expect_length(lit, 25L)
  line <- charToRaw("* 6 FETCH (UID 6 ENVELOPE (NIL {25}\r\n")
  old <- c(line, lit, charToRaw(rest))
  # pre-8.17
  w <- wire_rebuild(old, line)
  expect_identical(w$raw, old)
  # 8.17 and later, the tagged part read in the same transfer
  w <- wire_rebuild(c(line, charToRaw(rest)), c(line, lit))
  expect_identical(w$raw, old)
  # 8.17 and later, the tagged part still to come
  w <- wire_rebuild(line, c(line, lit))
  expect_identical(w$raw, c(line, lit))
})

test_that("wire_rebuild() copes with tiny and empty literals", {
  # a header field that does not exist: {2} and CRLF
  h <- "* 1 FETCH (UID 1 BODY[HEADER.FIELDS (X-NOPE)] {2}\r\n"
  w <- wire_rebuild(charToRaw(h), charToRaw(paste0(h, "\r\n")))
  expect_identical(wire_text(w$raw), paste0(h, "\r\n"))
  old <- paste0(h, "\r\n)\r\nA3 OK Fetch completed.\r\n")
  w <- wire_rebuild(charToRaw(old), charToRaw(h))
  expect_identical(wire_text(w$raw), old)
  # {0}
  old0 <- "* 7 FETCH (BODY[HEADER]<120000> {0}\r\n)\r\nA9 OK Fetch completed.\r\n"
  w <- wire_rebuild(charToRaw(old0), charToRaw("* 7 FETCH (BODY[HEADER]<120000> {0}\r\n"))
  expect_identical(wire_text(w$raw), old0)
  w <- wire_rebuild(charToRaw("* 7 FETCH (BODY[HEADER]<120000> {0}\r\n"),
                    charToRaw("* 7 FETCH (BODY[HEADER]<120000> {0}\r\n"))
  expect_identical(wire_text(w$raw), "* 7 FETCH (BODY[HEADER]<120000> {0}\r\n")
})

test_that("wire_rebuild() is not fooled by a multi-message reply of the old layout", {
  # pre-8.17 on "UID FETCH $ BODY.PEEK[HEADER.FIELDS (SUBJECT)]": the body
  # buffer holds the three announcing lines back to back, and the second one
  # is longer than the first literal
  l1 <- "* 1 FETCH (UID 1 BODY[HEADER.FIELDS (SUBJECT)] {29}\r\n"
  l2 <- "* 2 FETCH (UID 2 BODY[HEADER.FIELDS (SUBJECT)] {53}\r\n"
  l3 <- "* 3 FETCH (UID 3 BODY[HEADER.FIELDS (SUBJECT)] {36}\r\n"
  old <- paste0(l1, "Subject: Invoice attached\r\n\r\n)\r\n",
                l2, "Subject: =?UTF-8?B?UmVsYXTDs3JpbyBkZSB2ZW5kYXM=?=\r\n\r\n)\r\n",
                l3, "Subject: Re: Travel arrangements\r\n\r\n)\r\n",
                "A029 OK Fetch completed (0.001 + 0.000 secs).\r\n")
  w <- wire_rebuild(charToRaw(old), charToRaw(paste0(l1, l2, l3)))
  expect_identical(wire_text(w$raw), old)
  expect_false(w$missing)
})

test_that("wire_rebuild() flags a literal that was announced but not delivered", {
  w <- wire_rebuild(charToRaw("* 1 FETCH (UID 1 BODY[TEXT] {121}\r\n"), raw(0))
  expect_true(w$missing)
  w <- wire_rebuild(charToRaw("* 1 FETCH (UID 1 BODY[TEXT] {121}\r\nshort"), raw(0))
  expect_true(w$missing)
})

test_that("literal markers inside quoted strings are not literals", {
  expect_true(is.na(literal_marker(charToRaw("* 1 FETCH (BODY[HEADER] \"Subject: {50} Test\")\r\n"))))
  expect_true(is.na(literal_marker(charToRaw("* 456 FETCH ((\"fake {50}\")\r\n"))))
  expect_equal(literal_marker(charToRaw("* 456 FETCH ((\"fake {50}\" BODY[TEXT]) {7201}\r\n")), 7201L)
  expect_equal(literal_marker(charToRaw("* 1 FETCH (UID 1 PREVIEW {12}\r\n")), 12L)
  expect_equal(literal_marker(charToRaw("* 1 FETCH (BINARY[1] ~{12}\r\n")), 12L)
  expect_true(is.na(literal_marker(charToRaw("A1 OK done\r\n"))))
})

test_that("wire_split() drops the leftovers of an earlier reply", {
  # 8.17 and later: ")" and the tagged line of the previous FETCH arrive at
  # the head of the next command's reply
  x <- charToRaw(")\r\nA004 OK Fetch completed (0.001 + 0.000 secs).\r\nA005 OK NOOP completed (0.001 + 0.000 secs).\r\n")
  s <- wire_split(x, "A005")
  expect_identical(wire_text(s$reply), "A005 OK NOOP completed (0.001 + 0.000 secs).\r\n")
  expect_true(s$closed)
  expect_equal(s$stale, 50L)
  # the same without knowing the tag
  s <- wire_split(x, NULL)
  expect_identical(wire_text(s$reply), "A005 OK NOOP completed (0.001 + 0.000 secs).\r\n")
  expect_true(s$closed)
  # leftovers with literal lines of an earlier multi-literal reply
  x <- charToRaw(paste0(" BODY[TEXT] {121}\r\n", strrep("x", 121), ")\r\n",
                        "A010 OK Fetch completed (0.001 + 0.000 secs).\r\n",
                        "* 2 FETCH (UID 2 RFC822.SIZE 521)\r\n",
                        "A011 OK Fetch completed (0.001 + 0.000 secs).\r\n"))
  s <- wire_split(x, "A011")
  expect_identical(wire_text(s$reply),
                   "* 2 FETCH (UID 2 RFC822.SIZE 521)\r\nA011 OK Fetch completed (0.001 + 0.000 secs).\r\n")
  expect_true(s$closed)
})

test_that("wire_split() sees through tagged-looking lines inside a literal", {
  body <- "A003 OK this is mail text, not a reply\r\n"
  x <- charToRaw(paste0("* 1 FETCH (UID 1 BODY[TEXT] {", nchar(body), "}\r\n", body,
                        ")\r\nA004 OK Fetch completed.\r\n"))
  s <- wire_split(x, "A004")
  expect_equal(s$stale, 0L)
  expect_true(s$closed)
  expect_identical(s$reply, x)
})

test_that("wire_split() reports a reply that is not closed", {
  x <- charToRaw("* 1 FETCH (UID 1 BODY[TEXT] {5}\r\nhello")
  s <- wire_split(x, "A004")
  expect_false(s$closed)
  expect_false(s$missing)
  s <- wire_split(charToRaw("* 1 FETCH (UID 1 BODY[TEXT] {5}\r\nhe"), "A004")
  expect_true(s$missing)
  # closed by another command's tagged line is not closed
  s <- wire_split(charToRaw("* 1 FETCH (UID 1 BODY[TEXT] {5}\r\nhello)\r\nA003 OK done\r\n"), "A004")
  expect_false(s$closed)
})

test_that("wire_tag_end() finds where a command's tagged line ends", {
  x <- charToRaw(")\r\nA004 OK Fetch completed.\r\nA005 OK NOOP completed.\r\n")
  expect_equal(wire_tag_end(x, "A004"), 29L)
  expect_equal(wire_tag_end(x, "A005"), length(x))
  expect_equal(wire_tag_end(x, "A006"), 0L)
})

# curl_perform() on canned transfers: the normalization and the flush

canned_transfer <- function(headers, content = "", tag = NULL, epoch = NULL) {
  list(headers = charToRaw(headers), content = charToRaw(content), tag = tag, epoch = epoch)
}

# `self` with a debug log; every canned transfer sets the tag (and epoch) the
# debug callback would have recorded
run_canned <- function(transfers, .env = parent.frame()) {
  self <- new.env(parent = emptyenv())
  self$con_debug <- new.env(parent = emptyenv())
  self$con_debug$epoch <- 1L
  self$con_debug$tag <- "A003"
  i <- 0L
  sent <- character(0)
  testthat::local_mocked_bindings(
    perform_transfer = function(url, handle, self) {
      i <<- i + 1L
      tr <- transfers[[min(i, length(transfers))]]
      if (!is.null(tr$tag)) self$con_debug$tag <- tr$tag
      if (!is.null(tr$epoch)) self$con_debug$epoch <- tr$epoch
      list(headers = tr$headers, content = tr$content)
    },
    .package = "mRpostman", .env = .env)
  testthat::local_mocked_bindings(
    handle_setopt = function(handle, ...) {
      o <- list(...)
      if (!is.null(o$customrequest)) sent <<- c(sent, o$customrequest)
      invisible(handle)
    },
    .package = "curl", .env = .env)
  out <- curl_perform("imap://example.test", NULL, self)
  list(out = out, sent = sent, flushes = self$con_debug$flushes)
}

test_that("curl_perform() leaves a closed reply of the old layout alone", {
  r <- run_canned(list(canned_transfer(old_text, "* 1 FETCH (UID 1 BODY[TEXT] {121}\r\n", tag = "A004")))
  expect_identical(rawToChar(r$out$headers), old_text)
  expect_true(r$out$complete)
  expect_null(r$flushes)
  expect_length(r$sent, 0)
})

test_that("curl_perform() flushes a reply libcurl left open and stitches the rest", {
  h <- "* 1 FETCH (UID 1 BODY[TEXT] {121}\r\n"
  b <- paste0(h, "This supersedes the previous version sent last week.\r\n\r\n",
              "Let me know if you need any further details before the meeting.\r\n")
  r <- run_canned(list(
    canned_transfer(h, b, tag = "A004"),
    canned_transfer(")\r\nA004 OK Fetch completed (0.001 + 0.000 secs).\r\nA005 OK NOOP completed (0.001 + 0.000 secs).\r\n",
                    tag = "A005")))
  expect_identical(rawToChar(r$out$headers), old_text)
  expect_true(r$out$complete)
  expect_equal(r$flushes, 1L)
  expect_identical(r$sent, "NOOP")
})

test_that("curl_perform() recovers the other literals of a multi-literal reply", {
  # 8.23.0 on "UID FETCH 1 (BODY.PEEK[HEADER] BODY.PEEK[TEXT])": the second
  # literal comes with the NOOP, as header lines
  h <- "* 1 FETCH (UID 1 BODY[HEADER] {13}\r\n"
  b <- paste0(h, "From: a@b.c\r\n")
  r <- run_canned(list(
    canned_transfer(h, b, tag = "A004"),
    canned_transfer(paste0(" BODY[TEXT] {6}\r\nhello\r\n)\r\n",
                           "A004 OK Fetch completed (0.001 + 0.000 secs).\r\n",
                           "A005 OK NOOP completed (0.001 + 0.000 secs).\r\n"), tag = "A005")))
  expect_identical(rawToChar(r$out$headers),
                   paste0(h, "From: a@b.c\r\n BODY[TEXT] {6}\r\nhello\r\n)\r\n",
                          "A004 OK Fetch completed (0.001 + 0.000 secs).\r\n"))
  expect_true(r$out$complete)
})

test_that("curl_perform() stops flushing when libcurl replaced the connection", {
  # 8.18.0 closes the connection after some literal replies: the NOOP goes
  # out on a new one (greeting, login) and the tagged line never comes
  h <- "* 3 FETCH (UID 3 BODY[TEXT] {5}\r\n"
  r <- run_canned(list(
    canned_transfer(h, paste0(h, "hello"), tag = "A004"),
    canned_transfer("* OK Dovecot ready.\r\nB001 OK Pre-login capabilities listed.\r\nB002 OK Logged in\r\nB003 OK NOOP completed.\r\n",
                    tag = "B003", epoch = 2L)))
  expect_identical(rawToChar(r$out$headers), paste0(h, "hello"))
  expect_true(r$out$complete)
  expect_equal(r$flushes, 1L)
})

test_that("curl_perform() gives up on a reply that never closes", {
  h <- "* 1 FETCH (UID 1 BODY[TEXT] {5}\r\n"
  r <- run_canned(list(
    canned_transfer(h, paste0(h, "hello"), tag = "A004"),
    canned_transfer("A005 OK NOOP completed.\r\n", tag = "A005")))
  expect_false(r$out$complete)
  expect_equal(r$flushes, 8L)
})

test_that("curl_perform() drops the leftovers of the previous command", {
  r <- run_canned(list(canned_transfer(
    ")\r\nA004 OK Fetch completed (0.001 + 0.000 secs).\r\n* 2 FETCH (UID 2 RFC822.SIZE 521)\r\nA005 OK Fetch completed (0.001 + 0.000 secs).\r\n",
    "* 2 FETCH (UID 2 RFC822.SIZE 521)\r\n", tag = "A005")))
  expect_identical(rawToChar(r$out$headers),
                   "* 2 FETCH (UID 2 RFC822.SIZE 521)\r\nA005 OK Fetch completed (0.001 + 0.000 secs).\r\n")
  expect_true(r$out$complete)
})
