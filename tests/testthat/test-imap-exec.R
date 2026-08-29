# The execution choke point: request assembly, retry, and classed failures,
# all offline via the canned transport of helper-canned.R.

test_that("imap_exec sends the command and returns headers+content as text", {
  con <- fake_con()
  rec <- local_imap_transport(canned_response("* 3 EXISTS\r\n", "A1 OK done\r\n"))
  out <- mRpostman:::imap_exec(con, "NOOP", retries = 0)
  expect_identical(rec$requests, "NOOP")
  expect_identical(out$text, "* 3 EXISTS\r\n\r\nA1 OK done\r\n")
})

test_that("imap_exec re-sets the command before every attempt and retries", {
  con <- fake_con(folder = NA)
  rec <- local_imap_transport(
    function() stop("Timeout was reached: Operation timed out"),
    canned_response("* SEARCH 1 2\r\n"))
  out <- mRpostman:::imap_exec(con, "UID SEARCH ALL", retries = 1)
  # one set per attempt: the retry cannot rely on a stale handle option
  expect_identical(rec$requests, c("UID SEARCH ALL", "UID SEARCH ALL"))
  expect_match(out$text, "SEARCH 1 2")
})

test_that("needs_folder fails upfront with a classed state error", {
  con <- fake_con(folder = NA)
  expect_error(mRpostman:::imap_exec(con, "EXPUNGE", needs_folder = TRUE),
               class = "mRpostman_state_error")
})

test_that("a persistent failure raises a classed connection error", {
  con <- fake_con(folder = NA)
  local_imap_transport(function() stop("Timeout was reached: Operation timed out"))
  expect_error(mRpostman:::imap_exec(con, "NOOP", retries = 1),
               class = "mRpostman_connection_error")
})

test_that("a tagged NO recorded by the debug callback becomes a server error", {
  con <- fake_con()
  con$con_debug$lines <- c("* OK ready", "A7 NO [CANNOT] bad flag")
  local_imap_transport(function() stop("Quote command returned error"))
  err <- tryCatch(mRpostman:::imap_exec(con, "STORE 1 +FLAGS (\\Foo)", retries = 0),
                  error = function(e) e)
  expect_s3_class(err, "mRpostman_server_error")
  expect_s3_class(err, "mRpostman_error")
  expect_identical(err$server_reply, "NO [CANNOT] bad flag")
})

test_that("assert_capability raises a classed capability error", {
  con <- fake_con()
  con$server_capabilities <- c("IMAP4REV1", "IDLE")
  err <- tryCatch(
    mRpostman:::assert_capability(con, "ESEARCH", command = "search", rfc = "RFC 4731"),
    error = function(e) e)
  expect_s3_class(err, "mRpostman_capability_error")
  expect_identical(err$capability, "ESEARCH")
})

test_that("CURLE_TOO_LARGE surfaces as mRpostman_response_too_large", {
  con <- fake_con()
  local_imap_transport(function() stop("A value or data field grew larger than allowed"))
  expect_error(mRpostman:::imap_exec(con, "UID SEARCH UNSEEN", retries = 0),
               class = "mRpostman_response_too_large")
})
