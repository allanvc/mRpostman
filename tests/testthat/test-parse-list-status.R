# parse_list_status(): joins the untagged LIST and STATUS lines of a
# LIST ... RETURN (STATUS ...) response (LIST-STATUS, RFC 5819).

test_that("folders and their status items are joined into one data frame", {
  resp <- paste0(
    '* LIST (\\HasNoChildren) "/" "INBOX"\r\n',
    '* STATUS "INBOX" (MESSAGES 17 UNSEEN 16)\r\n',
    '* LIST (\\HasNoChildren) "/" "Sent Items"\r\n',
    '* STATUS "Sent Items" (MESSAGES 3 UNSEEN 0)\r\n',
    '* LIST (\\Noselect \\HasChildren) "/" "Archive"\r\n',
    '* LIST (\\HasNoChildren) "/" Archive/2020\r\n',
    '* STATUS Archive/2020 (UNSEEN 2 MESSAGES 40)\r\n',
    'A003 OK List completed (0.001 + 0.000 secs).\r\n')
  x <- mRpostman:::parse_list_status(resp, c("MESSAGES", "UNSEEN"))
  expect_s3_class(x, "data.frame")
  expect_identical(names(x), c("folder", "MESSAGES", "UNSEEN"))
  expect_identical(x$folder, c("INBOX", "Sent Items", "Archive/2020"))
  expect_equal(x$MESSAGES, c(17, 3, 40))
  expect_equal(x$UNSEEN, c(16, 0, 2))
})

test_that("a folder without a STATUS line gets NA counts", {
  resp <- paste0('* LIST () "/" "INBOX"\r\n',
                 '* LIST () "/" "Drafts"\r\n',
                 '* STATUS "INBOX" (MESSAGES 1)\r\n')
  x <- mRpostman:::parse_list_status(resp, "MESSAGES")
  expect_equal(x$MESSAGES, c(1, NA))
})

test_that("an empty response yields an empty data frame with the right columns", {
  x <- mRpostman:::parse_list_status("A1 OK List completed\r\n", c("MESSAGES", "UNSEEN"))
  expect_identical(nrow(x), 0L)
  expect_identical(names(x), c("folder", "MESSAGES", "UNSEEN"))
})

test_that("folders repeated across header and body buffers are listed once", {
  resp <- paste0('* LIST () "/" "INBOX"\r\n* STATUS "INBOX" (MESSAGES 4)\r\n',
                 '* LIST () "/" "INBOX"\r\n* STATUS "INBOX" (MESSAGES 4)\r\n')
  x <- mRpostman:::parse_list_status(resp, "MESSAGES")
  expect_identical(nrow(x), 1L)
  expect_equal(x$MESSAGES, 4)
})
