# parse_sort(): returns SORT ids in the server-provided order, WITHOUT
# re-sorting or running fix_search_stripping() (which would corrupt the order
# and mangle multi-digit ids).

test_that("SORT ids keep the server order (not ascending)", {
  x <- mRpostman:::parse_sort("* SORT 5 3 4 1 2\r\nA003 OK SORT completed\r\n")
  expect_identical(x, c(5L, 3L, 4L, 1L, 2L))
})

test_that("multi-digit ids keep their value and order", {
  x <- mRpostman:::parse_sort("* SORT 12 3 45\r\n")
  expect_identical(x, c(12L, 3L, 45L))
})

test_that("an empty or absent SORT result yields integer(0)", {
  expect_identical(mRpostman:::parse_sort("* SORT\r\nA1 OK\r\n"), integer(0))
  expect_identical(mRpostman:::parse_sort("A1 OK done\r\n"), integer(0))
})

test_that("only the SORT line is parsed, even when the transport repeats it", {
  text <- paste0("* SORT 5 3 1\r\nA006 OK Sort completed (0.020 + 0.000 secs).\r\n",
                 "\r\n* SORT 5 3 1\r\n")
  expect_identical(mRpostman:::parse_sort(text), c(5L, 3L, 1L))
})
