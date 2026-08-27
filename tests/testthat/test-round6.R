# 1.5.5: CONDSTORE/QRESYNC response parsing and METADATA.

test_that("parse_modified() and parse_highestmodseq() read their response codes", {
  expect_identical(mRpostman:::parse_modified("A3 OK [MODIFIED 7,9:10] Conditional STORE failed\r\n"), c(7L, 9L, 10L))
  expect_identical(mRpostman:::parse_modified("A3 OK Store completed\r\n"), integer(0))
  expect_identical(mRpostman:::parse_highestmodseq("* OK [HIGHESTMODSEQ 12701549] Highest\r\n"), 12701549)
  expect_true(is.na(mRpostman:::parse_highestmodseq("A1 OK done\r\n")))
})

test_that("parse_resync() reads VANISHED and changed-flag lines", {
  resp <- paste0("* 5 EXISTS\r\n* OK [UIDVALIDITY 1783866848] UIDs valid\r\n* OK [UIDNEXT 12] Predicted\r\n",
                 "* OK [HIGHESTMODSEQ 340] Highest\r\n* VANISHED (EARLIER) 3:4,9\r\n",
                 "* 1 FETCH (UID 2 FLAGS (\\Seen \\Flagged) MODSEQ (331))\r\n",
                 "* 4 FETCH (UID 10 FLAGS () MODSEQ (340))\r\n",
                 "* 4 FETCH (UID 10 FLAGS () MODSEQ (340))\r\n",
                 "A2 OK [READ-WRITE] Select completed\r\n")
  x <- mRpostman:::parse_resync(resp)
  expect_identical(x$vanished, c(3L, 4L, 9L))
  expect_identical(x$changed$uid, c(2L, 10L))
  expect_identical(x$changed$flags, c("\\Seen \\Flagged", ""))
  expect_identical(x$changed$modseq, c(331, 340))
  expect_identical(x$highestmodseq, 340); expect_identical(x$uidvalidity, 1783866848)
  expect_identical(x$exists, 5)
  y <- mRpostman:::parse_resync("A2 OK Fetch completed\r\n")
  expect_identical(y$vanished, integer(0)); expect_identical(nrow(y$changed), 0L)
})

test_that("parse_metadata() reads entries, NIL values, and literals", {
  resp <- paste0('* METADATA "INBOX" (/private/comment "reviewed" /shared/comment NIL)\r\n',
                 '* METADATA "" ("/shared/vendor/x" {5}\r\nhello)\r\nA3 OK Getmetadata completed\r\n')
  x <- mRpostman:::parse_metadata(resp)
  expect_identical(x$mailbox, c("INBOX", "INBOX", ""))
  expect_identical(x$entry, c("/private/comment", "/shared/comment", "/shared/vendor/x"))
  expect_identical(x$value, c("reviewed", NA, "hello"))
  expect_identical(nrow(mRpostman:::parse_metadata("A3 OK\r\n")), 0L)
})

test_that("imap_quote() escapes quotes and backslashes", {
  expect_identical(mRpostman:::imap_quote('say "hi" \\ there'), '"say \\"hi\\" \\\\ there"')
})
