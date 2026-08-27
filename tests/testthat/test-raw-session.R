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

sandbox_up <- function() {
  ok <- tryCatch({
    s <- mRpostman:::imap_socket_open("imap://localhost:1430", 2000, FALSE)
    mRpostman:::imap_socket_close(s); TRUE
  }, error = function(e) FALSE)
  ok
}

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
