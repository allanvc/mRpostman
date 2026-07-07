# parse_id(): turns "* ID (\"name\" \"value\" ...)" (RFC 2971) into a named
# character vector; "* ID NIL" yields an empty named vector.

test_that("ID fields are parsed into a named character vector", {
  x <- mRpostman:::parse_id('* ID ("name" "Cyrus" "version" "1.5")\r\nA1 OK ID completed\r\n')
  expect_identical(x[["name"]], "Cyrus")
  expect_identical(x[["version"]], "1.5")
  expect_identical(names(x), c("name", "version"))
})

test_that("ID NIL yields an empty named vector", {
  x <- mRpostman:::parse_id("* ID NIL\r\n")
  expect_length(x, 0L)
  expect_type(x, "character")
})

test_that("no ID line yields an empty vector", {
  expect_length(mRpostman:::parse_id("A1 OK done\r\n"), 0L)
})
