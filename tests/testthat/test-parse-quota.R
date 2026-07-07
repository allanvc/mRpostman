# parse_quota(): turns "* QUOTA <root> (<res> <usage> <limit> ...)" responses
# (RFC 2087) into one row per resource, ignoring "* QUOTAROOT" lines.

test_that("a single-resource QUOTA is parsed", {
  x <- mRpostman:::parse_quota('* QUOTA "" (STORAGE 512 1024)\r\nA1 OK GETQUOTA completed\r\n')
  expect_s3_class(x, "data.frame")
  expect_identical(x$quota_root, "")
  expect_identical(x$resource, "STORAGE")
  expect_identical(x$usage, 512)
  expect_identical(x$limit, 1024)
})

test_that("GETQUOTAROOT output (QUOTAROOT + multi-resource QUOTA) is parsed", {
  resp <- paste0('* QUOTAROOT "INBOX" ""\r\n',
                 '* QUOTA "" (STORAGE 512 1024 MESSAGE 10 500)\r\n')
  x <- mRpostman:::parse_quota(resp)
  expect_identical(x$resource, c("STORAGE", "MESSAGE"))
  expect_identical(x$usage, c(512, 10))
  expect_identical(x$limit, c(1024, 500))
  expect_identical(x$quota_root, c("", ""))
})

test_that("no QUOTA line yields an empty data.frame", {
  x <- mRpostman:::parse_quota("A1 OK done\r\n")
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 0L)
})
