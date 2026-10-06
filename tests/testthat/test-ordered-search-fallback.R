# SORT and THREAD answer in one untagged line carrying every id. When that line
# is longer than libcurl accepts (CURLE_TOO_LARGE), execute_ordered_search()
# re-issues the command on the raw-socket session. Both transports are mocked:
# libcurl through the canned transport of helper-canned.R, the raw session
# through its four entry points.

too_large <- function() {
  stop("Failed to read response: the response line grew larger than allowed")
}

local_fake_raw <- function(lines, .env = parent.frame()) {
  rec <- new.env(parent = emptyenv())
  rec$commands <- character(0)
  rec$closed <- FALSE
  testthat::local_mocked_bindings(
    raw_session_start = function(self, auth, compress = FALSE, timeout_ms = 30000) {
      rec$auth <- auth
      new.env(parent = emptyenv())
    },
    raw_select = function(sess, folder) {
      rec$folder <- folder
      invisible(NULL)
    },
    raw_command = function(sess, command, literals = list(), timeout_ms = NULL) {
      rec$commands <- c(rec$commands, command)
      list(status = "OK", tagged = "R002 OK completed", lines = lines,
           literals = list())
    },
    raw_session_close = function(sess) {
      rec$closed <- TRUE
      invisible(TRUE)
    },
    .package = "mRpostman", .env = .env)
  rec
}

run_ordered <- function(con, cmd, parser, auth) {
  mRpostman:::execute_ordered_search(con, con$con_params$url, con$con_handle,
                                     cmd, parser, retries = 0, auth = auth)
}

test_that("a THREAD too large for libcurl is re-issued on the raw socket", {
  con <- fake_con(folder = "kaminski-v")
  curl_rec <- local_imap_transport(too_large)
  raw_rec <- local_fake_raw("* THREAD (1 2)(3 (4 5))")

  out <- run_ordered(con, "UID THREAD REFERENCES UTF-8 ALL",
                     mRpostman:::parse_thread, auth = list(user = "enron"))

  expect_identical(out, list(c(1L, 2L), c(3L, 4L, 5L)))
  expect_identical(curl_rec$requests, "UID THREAD REFERENCES UTF-8 ALL")
  expect_identical(raw_rec$commands, "UID THREAD REFERENCES UTF-8 ALL")
  expect_identical(raw_rec$folder, "kaminski-v")
  expect_identical(raw_rec$auth, list(user = "enron"))
  expect_true(raw_rec$closed)
})

test_that("a SORT too large for libcurl is re-issued on the raw socket", {
  con <- fake_con()
  local_imap_transport(too_large)
  raw_rec <- local_fake_raw("* SORT 5 3 1")

  out <- run_ordered(con, "UID SORT (DATE) UTF-8 ALL", mRpostman:::parse_sort,
                     auth = list(user = "enron"))

  expect_identical(out, c(5L, 3L, 1L))
  expect_identical(raw_rec$commands, "UID SORT (DATE) UTF-8 ALL")
  expect_identical(raw_rec$folder, "INBOX")
})

test_that("without credentials the too-large error propagates unchanged", {
  con <- fake_con()
  local_imap_transport(too_large)
  raw_rec <- local_fake_raw("* SORT 5 3 1")

  expect_error(run_ordered(con, "UID SORT (DATE) UTF-8 ALL",
                           mRpostman:::parse_sort, auth = NULL),
               class = "mRpostman_response_too_large")
  expect_length(raw_rec$commands, 0L)
})

test_that("a response libcurl can read never touches the raw socket", {
  con <- fake_con()
  local_imap_transport(canned_response("* SORT 2 1\r\n", "A1 OK Sort completed\r\n"))
  raw_rec <- local_fake_raw("* SORT 9 9 9")

  out <- run_ordered(con, "UID SORT (DATE) UTF-8 ALL", mRpostman:::parse_sort,
                     auth = list(user = "enron"))

  expect_identical(out, c(2L, 1L))
  expect_length(raw_rec$commands, 0L)
})

test_that("a NO on the raw socket is a server error, and the session is closed", {
  con <- fake_con()
  local_imap_transport(too_large)
  raw_rec <- local_fake_raw("* SORT 5 3 1")
  testthat::local_mocked_bindings(
    raw_command = function(sess, command, literals = list(), timeout_ms = NULL) {
      raw_rec$commands <- c(raw_rec$commands, command)
      list(status = "NO", tagged = "R002 NO [LIMIT] Too many results",
           lines = character(0), literals = list())
    },
    .package = "mRpostman")

  expect_error(run_ordered(con, "UID SORT (DATE) UTF-8 ALL",
                           mRpostman:::parse_sort, auth = list(user = "enron")),
               "Too many results")
  expect_true(raw_rec$closed)
})
