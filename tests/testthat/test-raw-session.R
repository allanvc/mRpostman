# 2.0.0: the raw socket session (offline parsing helpers, and a live round
# trip against the Docker sandbox when it is reachable).

test_that("raw_idle_events() parses unsolicited responses", {
  ev <- mRpostman:::raw_idle_events(c("* 7 EXISTS", "* 4 RECENT", "* 2 EXPUNGE",
                                      "* 3 FETCH (FLAGS (\\Seen))", "R004 OK IDLE terminated"))
  expect_identical(ev$type, c("EXISTS", "RECENT", "EXPUNGE", "FETCH"))
  expect_identical(ev$id, c(7L, 4L, 2L, 3L))
  expect_identical(ev$detail[4], "(FLAGS (\\Seen))")
  expect_identical(nrow(mRpostman:::raw_idle_events(character(0))), 0L)
})

test_that("raw_capability_tokens() reads CAPABILITY lines and response codes", {
  expect_identical(mRpostman:::raw_capability_tokens("* OK [CAPABILITY IMAP4rev1 IDLE SASL-IR] Dovecot ready."),
                   c("IMAP4rev1", "IDLE", "SASL-IR"))
  expect_identical(mRpostman:::raw_capability_tokens("* CAPABILITY IMAP4rev1 UNSELECT\r\n"),
                   c("IMAP4rev1", "UNSELECT"))
  expect_identical(mRpostman:::raw_capability_tokens("A1 OK done"), character(0))
})


test_that("the raw session logs in, appends with MULTIAPPEND, and idles (sandbox)", {
  skip_if_not(sandbox_up(), "Docker sandbox not reachable on localhost:1430")
  con <- configure_imap(url = "imap://localhost:1430", username = "rawtest",
                        password = "sandbox", use_ssl = FALSE)
  msgs <- vapply(1:2, function(i) paste0("Subject: raw ", i, "\r\n\r\nbody\r\n"), "")
  uids <- con$append_msgs(msgs, folder = "INBOX", flags = "Seen", mute = TRUE)
  expect_length(uids, 2); expect_false(anyNA(uids))
  con$select_folder("INBOX", mute = TRUE)
  ev <- con$idle(timeout = 2)
  expect_s3_class(ev, "data.frame")
  expect_identical(names(ev), c("type", "id", "detail"))
  con$disconnect()
})

test_that("the zlib streams round-trip data incrementally (COMPRESS=DEFLATE)", {
  d <- .Call(mRpostman:::C_zstream_new, FALSE, 6L)
  i <- .Call(mRpostman:::C_zstream_new, TRUE, 6L)
  msg1 <- charToRaw(paste(rep("A1 NOOP\r\n", 50), collapse = ""))
  msg2 <- charToRaw("A2 LOGOUT\r\n")
  c1 <- .Call(mRpostman:::C_zstream_deflate, d, msg1)
  c2 <- .Call(mRpostman:::C_zstream_deflate, d, msg2)
  expect_lt(length(c1), length(msg1))
  expect_identical(.Call(mRpostman:::C_zstream_inflate, i, c1), msg1)     # flushed per command
  expect_identical(.Call(mRpostman:::C_zstream_inflate, i, c2), msg2)
  # split delivery: inflate half of a block, then the rest
  half <- length(c1) %/% 2
  i2 <- .Call(mRpostman:::C_zstream_new, TRUE, 6L)
  a <- .Call(mRpostman:::C_zstream_inflate, i2, c1[seq_len(half)])
  b <- .Call(mRpostman:::C_zstream_inflate, i2, c1[-seq_len(half)])
  expect_identical(c(a, b), msg1)
})

test_that("raw_idle_events() also reads NOTIFY's STATUS and LIST lines", {
  ev <- mRpostman:::raw_idle_events(c('* STATUS "Archive" (MESSAGES 12 UIDNEXT 40)',
                                      '* LIST (\\HasNoChildren) "." NewFolder', "* 3 EXISTS"))
  expect_identical(ev$type, c("EXISTS", "STATUS", "LIST"))
  expect_true(is.na(ev$id[2]))
  expect_match(ev$detail[2], "MESSAGES 12")
})

test_that("imap_url() builds RFC 5092 relative URLs", {
  expect_identical(unclass(imap_url("INBOX", 12)), "/INBOX/;UID=12")
  expect_identical(unclass(imap_url("INBOX", 12, section = "HEADER")), "/INBOX/;UID=12/;SECTION=HEADER")
  expect_identical(unclass(imap_url("École", 5)), "/%26AMk-cole/;UID=5")
})

test_that("BINARY, CATENATE, NOTIFY, and COMPRESS work on the sandbox", {
  skip_if_not(sandbox_up(), "Docker sandbox not reachable on localhost:1430")
  con <- configure_imap(url = "imap://localhost:1430", username = "rawtest2",
                        password = "sandbox", use_ssl = FALSE)
  png <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 1, 2, 3, 4))
  msg <- paste0("From: a@b.c\r\nTo: d@e.f\r\nSubject: bin\r\nMIME-Version: 1.0\r\n",
                "Content-Type: multipart/mixed; boundary=\"b\"\r\n\r\n--b\r\nContent-Type: text/plain\r\n\r\nhello\r\n",
                "--b\r\nContent-Type: image/png; name=\"p.png\"\r\nContent-Transfer-Encoding: base64\r\n\r\n",
                base64enc::base64encode(png), "\r\n--b--\r\n")
  uid <- con$append_msg(msg, folder = "INBOX", mute = TRUE)
  con$select_folder("INBOX", mute = TRUE)
  b <- con$fetch_binary(msg_id = uid, part = "2", use_uid = TRUE)
  expect_identical(b[[1]], png)
  b2 <- con$fetch_binary(msg_id = uid, part = "2", use_uid = TRUE, compress = TRUE)
  expect_identical(b2[[1]], png)
  cu <- con$append_catenate(parts = list("Subject: catenated\r\n\r\n", imap_url("INBOX", uid, section = "TEXT")),
                            folder = "INBOX")
  expect_false(is.na(cu))
  expect_match(con$fetch_text(msg_id = cu, use_uid = TRUE)[[1]], "hello")
  ev <- con$notify(mailboxes = "personal", timeout = 2)
  expect_true(any(ev$type == "STATUS"))
  con$disconnect()
})
