# ACL (RFC 4314), ESORT (RFC 5267), ENABLE (RFC 5161), and SEARCHRES (RFC 5182)
# request/response helpers.

test_that("parse_acl() returns one row per identifier", {
  resp <- '* ACL "INBOX" tour150 lrwstipekxa anyone lrs\r\nA3 OK Getacl completed.\r\n'
  x <- mRpostman:::parse_acl(resp)
  expect_identical(x$identifier, c("tour150", "anyone"))
  expect_identical(x$rights, c("lrwstipekxa", "lrs"))
  # duplicated lines (header + body buffers) are not double-counted
  expect_identical(nrow(mRpostman:::parse_acl(paste0(resp, resp))), 2L)
  expect_identical(nrow(mRpostman:::parse_acl("A3 OK done\r\n")), 0L)
})

test_that("parse_listrights() and parse_myrights() read their lines", {
  lr <- mRpostman:::parse_listrights('* LISTRIGHTS INBOX anyone "" l r w s t p i e k x a\r\n')
  expect_identical(lr$required, "")
  expect_identical(lr$optional, c("l","r","w","s","t","p","i","e","k","x","a"))
  expect_null(mRpostman:::parse_listrights("A1 OK\r\n"))
  expect_identical(mRpostman:::parse_myrights('* MYRIGHTS "Sent Items" lrwstipekxa\r\n'),
                   "lrwstipekxa")
  expect_true(is.na(mRpostman:::parse_myrights("A1 OK\r\n")))
})

test_that("parse_esort() reads COUNT/MIN/MAX and an ordered ALL set", {
  x <- mRpostman:::parse_esort('* ESEARCH (TAG "A5") COUNT 4 MIN 2 MAX 9 ALL 9:7,2\r\n')
  expect_identical(x$count, 4L); expect_identical(x$min, 2L); expect_identical(x$max, 9L)
  expect_identical(x$all, c(9L, 8L, 7L, 2L))
  expect_identical(mRpostman:::parse_esort("A5 OK Sort completed\r\n"), list())
})

test_that("parse_enabled() lists the confirmed extensions", {
  expect_identical(mRpostman:::parse_enabled("* ENABLED CONDSTORE QRESYNC\r\nA2 OK\r\n"),
                   c("CONDSTORE", "QRESYNC"))
  expect_identical(mRpostman:::parse_enabled("* ENABLED\r\nA2 OK\r\n"), character(0))
})

test_that("search(save = TRUE) builds a SEARCH RETURN (SAVE) request", {
  h <- curl::new_handle()
  out <- mRpostman:::define_searchrequest_custom("FLAGGED", negate = FALSE,
                                                 use_uid = TRUE, esearch = FALSE,
                                                 handle = h, save = TRUE)
  expect_identical(unname(out$customrequest), "UID SEARCH RETURN (SAVE) (FLAGGED)")
})

test_that("parse_quota() keeps one row per root and resource", {
  resp <- '* QUOTA "User quota" (STORAGE 510 1048576)\r\n* QUOTA "User quota" (STORAGE 510 1048576)\r\n'
  expect_identical(nrow(mRpostman:::parse_quota(resp)), 1L)
})

test_that("split_fetch_responses() yields one cleaned element per message", {
  raw <- paste0(
    "* 2 FETCH (BODY[HEADER.FIELDS (SUBJECT)] {14}\r\nSubject: two\r\n)\r\n",
    "* 5 FETCH (UID 50 BODY[HEADER.FIELDS (SUBJECT)] {15}\r\nSubject: five\r\n)\r\n",
    "A3 OK Fetch completed (0.001 + 0.000 secs).\r\n")
  x <- mRpostman:::split_fetch_responses(raw, "header")
  expect_identical(names(x), c("header2", "header5"))
  expect_identical(x[["header2"]], "Subject: two\r\n")
  expect_identical(x[["header5"]], "Subject: five\r\n")
  y <- mRpostman:::split_fetch_responses(raw, "header", use_uid = TRUE)
  expect_identical(names(y)[2], "headerUID50")
  expect_identical(mRpostman:::split_fetch_responses("A3 OK\r\n", "body"), list())
})
