# parse_status_counts(): extracts the (KEY value ...) data items from an
# untagged "* STATUS <folder> (...)" response, labeled by keyword and
# order-independent.

test_that("all status items are parsed and named correctly", {
  resp <- paste0('* STATUS "INBOX" (MESSAGES 231 RECENT 0 UIDNEXT 44292 ',
                 'UIDVALIDITY 1 UNSEEN 3)\r\nA002 OK STATUS completed\r\n')
  x <- mRpostman:::parse_status_counts(resp)
  expect_type(x, "double")
  expect_equal(unname(x[["MESSAGES"]]), 231)
  expect_equal(unname(x[["RECENT"]]), 0)
  expect_equal(unname(x[["UIDNEXT"]]), 44292)
  expect_equal(unname(x[["UIDVALIDITY"]]), 1)
  expect_equal(unname(x[["UNSEEN"]]), 3)
  expect_identical(names(x),
                   c("MESSAGES", "RECENT", "UIDNEXT", "UIDVALIDITY", "UNSEEN"))
})

test_that("a subset of items (and an unquoted folder name) is handled", {
  resp <- '* STATUS Sent (MESSAGES 12 UNSEEN 0)\r\n'
  x <- mRpostman:::parse_status_counts(resp)
  expect_identical(as.numeric(x), c(12, 0))
  expect_identical(names(x), c("MESSAGES", "UNSEEN"))
})

test_that("items in a different order are labeled by keyword, not position", {
  resp <- '* STATUS "INBOX" (UNSEEN 7 MESSAGES 40)\r\n'
  x <- mRpostman:::parse_status_counts(resp)
  expect_equal(unname(x[["MESSAGES"]]), 40)
  expect_equal(unname(x[["UNSEEN"]]), 7)
})

test_that("no STATUS list yields an empty numeric vector", {
  expect_length(mRpostman:::parse_status_counts("A001 OK done\r\n"), 0L)
})

test_that("STATUS is parsed even when a reconnection prepends CAPABILITY/AUTH lines", {
  # Regression: after a stale-connection reconnect, the buffer also holds the
  # CAPABILITY line (which contains the LIST-STATUS token) and the
  # "... authenticated (Success)" line. The parser must still lock onto the
  # untagged "* STATUS" response and not onto "LIST-STATUS" + "(Success)".
  resp <- paste0(
    "* CAPABILITY IMAP4rev1 UNSELECT IDLE NAMESPACE QUOTA ID XLIST CHILDREN ",
    "X-GM-EXT-1 UIDPLUS COMPRESS=DEFLATE ENABLE MOVE CONDSTORE ESEARCH ",
    "UTF8=ACCEPT LIST-EXTENDED LIST-STATUS LITERAL- SPECIAL-USE ",
    "APPENDLIMIT=35651584\r\n",
    "O002 OK quadros.ksu@gmail.com authenticated (Success)\r\n",
    '* STATUS "INBOX" (MESSAGES 11183 RECENT 0 UIDNEXT 12481 UIDVALIDITY 1 ',
    "UNSEEN 5330)\r\nO003 OK Success\r\n")
  x <- mRpostman:::parse_status_counts(resp)
  expect_equal(unname(x[["MESSAGES"]]), 11183)
  expect_equal(unname(x[["RECENT"]]), 0)
  expect_equal(unname(x[["UIDNEXT"]]), 12481)
  expect_equal(unname(x[["UIDVALIDITY"]]), 1)
  expect_equal(unname(x[["UNSEEN"]]), 5330)
  expect_identical(names(x),
                   c("MESSAGES", "RECENT", "UIDNEXT", "UIDVALIDITY", "UNSEEN"))
})
