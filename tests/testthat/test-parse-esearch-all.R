# parse_esearch_all(): expands the ESEARCH "ALL <sequence-set>" without eval(),
# replacing the previous eval(parse()) that ran server-provided text as R code.

test_that("sequence-sets with ranges and lists expand correctly", {
  expect_identical(
    mRpostman:::parse_esearch_all('* ESEARCH (TAG "A2") ALL 1:3,5,9:11\r\n'),
    c(1L, 2L, 3L, 5L, 9L, 10L, 11L))
})

test_that("a single id is parsed", {
  expect_identical(mRpostman:::parse_esearch_all('* ESEARCH (TAG "A2") ALL 7\r\n'), 7L)
})

test_that("an empty search (no ALL) returns integer(0)", {
  expect_identical(mRpostman:::parse_esearch_all('* ESEARCH (TAG "A2")\r\n'), integer(0))
})

test_that("malicious server text is not evaluated as code", {
  # the previous eval(parse()) implementation would execute this; the parser must not
  out <- mRpostman:::parse_esearch_all('* ESEARCH (TAG "A2") ALL 1); stop("INJECTED"); c(2\r\n')
  expect_identical(out, 1L)
})
