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

test_that("only the THREAD line is parsed, even when the transport repeats it", {
  # what imap_exec() hands over on the libcurl path: the untagged line among
  # the headers, the tagged OK line (with decimals), then the same untagged
  # line again as the body
  line <- "* THREAD (1)(2 3)(4 (5)(6 7))"
  text <- paste0(line, "\r\nA006 OK Thread completed (0.020 + 0.000 + 0.019 secs).\r\n",
                 "\r\n", line, "\r\n")
  expect_identical(mRpostman:::parse_thread(text), mRpostman:::parse_thread(line))
  expect_identical(mRpostman:::parse_thread(text),
                   list(1L, c(2L, 3L), c(4L, 5L, 6L, 7L)))
  expect_false(any(unlist(mRpostman:::parse_thread(text)) == 0L))
})

test_that("an empty THREAD response yields an empty list", {
  expect_identical(mRpostman:::parse_thread("* THREAD\r\nA1 OK done\r\n"), list())
})
