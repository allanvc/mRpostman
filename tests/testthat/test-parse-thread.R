# parse_thread(): splits the top-level parenthesized groups of a THREAD
# response and flattens each into an integer vector of that thread's ids.

test_that("flat threads become a list of single-id vectors", {
  x <- mRpostman:::parse_thread("* THREAD (1)(2)(3)\r\n")
  expect_equal(x, list(1L, 2L, 3L))
})

test_that("grouped and nested threads flatten per top-level thread", {
  x <- mRpostman:::parse_thread("* THREAD (1)(2 3)(4 5 (6)(7))\r\nA1 OK THREAD completed\r\n")
  expect_equal(x, list(1L, c(2L, 3L), c(4L, 5L, 6L, 7L)))
})

test_that("no THREAD response yields an empty list", {
  expect_equal(mRpostman:::parse_thread("A1 OK done\r\n"), list())
})
