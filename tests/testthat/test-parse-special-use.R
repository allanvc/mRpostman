# parse_special_use(): pulls the folder name and its special-use attribute
# from each "* LIST (...) sep name" line, skipping folders without one.

test_that("special-use folders are extracted with their attribute", {
  resp <- paste0(
    '* LIST (\\HasNoChildren \\Sent) "/" "[Gmail]/Sent Mail"\r\n',
    '* LIST (\\HasNoChildren \\Trash) "/" "[Gmail]/Trash"\r\n',
    '* LIST (\\HasNoChildren \\Drafts) "/" "Drafts"\r\n')
  x <- mRpostman:::parse_special_use(resp)
  expect_s3_class(x, "data.frame")
  expect_identical(x$folder,
                   c("[Gmail]/Sent Mail", "[Gmail]/Trash", "Drafts"))
  expect_identical(x$special_use, c("\\Sent", "\\Trash", "\\Drafts"))
})

test_that("folders without a special-use attribute are skipped", {
  resp <- paste0(
    '* LIST (\\HasNoChildren) "/" "INBOX"\r\n',
    '* LIST (\\HasNoChildren \\Junk) "/" "Spam"\r\n')
  x <- mRpostman:::parse_special_use(resp)
  expect_identical(x$folder, "Spam")
  expect_identical(x$special_use, "\\Junk")
})

test_that("no special-use folders yields an empty data.frame", {
  x <- mRpostman:::parse_special_use("A001 OK done\r\n")
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 0L)
})
