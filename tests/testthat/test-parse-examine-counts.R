# parse_examine_counts(): labels EXISTS/RECENT by keyword, tolerant of a
# missing or reordered RECENT (which the previous positional code did not handle).

test_that("EXISTS and RECENT are parsed and named correctly", {
  x <- mRpostman:::parse_examine_counts("* 172 EXISTS\r\n* 3 RECENT\r\n")
  expect_identical(as.integer(x), c(172L, 3L))
  expect_identical(names(x), c("EXISTS", "RECENT"))
})

test_that("a missing RECENT does not error (returns EXISTS only)", {
  x <- mRpostman:::parse_examine_counts("* 42 EXISTS\r\nA1 OK [READ-ONLY] EXAMINE completed\r\n")
  expect_identical(as.integer(x), 42L)
  expect_identical(names(x), "EXISTS")
})

test_that("reordered responses are labeled by keyword, not by position", {
  x <- mRpostman:::parse_examine_counts("* 0 RECENT\r\n* 5 EXISTS\r\n")
  expect_equal(unname(x[["EXISTS"]]), 5)
  expect_equal(unname(x[["RECENT"]]), 0)
  # stable ordering: EXISTS first
  expect_identical(names(x), c("EXISTS", "RECENT"))
})

test_that("unrelated digits (e.g. UIDNEXT) are not captured", {
  x <- mRpostman:::parse_examine_counts(
    "A1 OK [UIDNEXT 9999] [UIDVALIDITY 12345]\r\n* 10 EXISTS\r\n* 2 RECENT\r\n")
  expect_identical(as.integer(x), c(10L, 2L))
})

test_that("no counts yields an empty numeric vector", {
  expect_length(mRpostman:::parse_examine_counts("A1 OK done\r\n"), 0L)
})
