# SAVEDATE / CONDSTORE criteria, plain SEARCH id extraction, PREVIEW, and
# LIST-EXTENDED parsing.

test_that("saved_*() and modseq() build their search keys", {
  expect_identical(saved_since("27-Aug-2026"), "(SAVEDSINCE 27-Aug-2026)")
  expect_identical(saved_before("01-Jan-2020", negate = TRUE), "(NOT (SAVEDBEFORE 01-Jan-2020))")
  expect_identical(saved_on("15-Mar-2021"), "(SAVEDON 15-Mar-2021)")
  expect_identical(modseq(205), "(MODSEQ 205)")
  expect_identical(modseq(1e6), "(MODSEQ 1000000)")
  expect_error(modseq(-1))
})

test_that("parse_search_ids() drops the trailing (MODSEQ n) item", {
  x <- mRpostman:::parse_search_ids("* SEARCH 3 7 12 (MODSEQ 205)\r\nA7 OK Search completed\r\n")
  expect_identical(x, c(3, 7, 12))
  expect_identical(mRpostman:::parse_search_ids("* SEARCH\r\nA7 OK\r\n"), numeric(0))
  expect_identical(mRpostman:::parse_search_ids("A7 OK Search completed (0.001 secs).\r\n"), numeric(0))
})

test_that("parse_preview() handles quoted, escaped, literal, and NIL forms", {
  expect_identical(mRpostman:::parse_preview('PREVIEW "Could you confirm?" SAVEDATE "x"'), "Could you confirm?")
  expect_identical(mRpostman:::parse_preview('PREVIEW "He said \\"hi\\" to me"'), 'He said "hi" to me')
  expect_identical(mRpostman:::parse_preview("PREVIEW {5}\r\nhello\r\n"), "hello")
  expect_true(is.na(mRpostman:::parse_preview("PREVIEW NIL")))
})

test_that("parse_list_extended() returns folder attributes", {
  resp <- paste0('* LIST (\\HasNoChildren \\Subscribed) "." INBOX\r\n',
                 '* LIST (\\HasChildren \\Noselect) "." "Archive"\r\n',
                 '* LIST (\\HasNoChildren \\Sent) "." "Sent Items"\r\n',
                 '* LIST (\\HasNoChildren \\Subscribed) "." INBOX\r\n',
                 'A1 OK List completed\r\n')
  x <- mRpostman:::parse_list_extended(resp)
  expect_identical(x$folder, c("INBOX", "Archive", "Sent Items"))
  expect_identical(x$delimiter, rep(".", 3))
  expect_identical(x$selectable, c(TRUE, FALSE, TRUE))
  expect_identical(x$has_children, c(FALSE, TRUE, FALSE))
  expect_identical(x$subscribed, c(TRUE, FALSE, FALSE))
  expect_identical(x$special_use, c(NA, NA, "\\Sent"))
})
