# parse_namespace(): splits the three NAMESPACE components (personal /
# other-users / shared), each NIL or a list of ("prefix" "delimiter") pairs.

test_that("the three components are parsed, with NIL as NULL", {
  resp <- paste0('* NAMESPACE (("" "/")) (("Other Users/" "/")) NIL\r\n',
                 'A001 OK NAMESPACE completed\r\n')
  x <- mRpostman:::parse_namespace(resp)
  expect_identical(x$personal$prefix, "")
  expect_identical(x$personal$delimiter, "/")
  expect_identical(x$other_users$prefix, "Other Users/")
  expect_identical(x$other_users$delimiter, "/")
  expect_null(x$shared)
})

test_that("multiple personal prefixes and a NIL delimiter are handled", {
  resp <- '* NAMESPACE (("" "/")("#news." ".")) NIL NIL\r\n'
  x <- mRpostman:::parse_namespace(resp)
  expect_identical(x$personal$prefix, c("", "#news."))
  expect_identical(x$personal$delimiter, c("/", "."))
  expect_null(x$other_users)
  expect_null(x$shared)
})

test_that("a NIL delimiter yields NA in the delimiter column", {
  resp <- '* NAMESPACE (("" NIL)) NIL NIL\r\n'
  x <- mRpostman:::parse_namespace(resp)
  expect_identical(x$personal$prefix, "")
  expect_true(is.na(x$personal$delimiter))
})

test_that("no NAMESPACE line yields all-NULL components", {
  x <- mRpostman:::parse_namespace("A001 OK done\r\n")
  expect_null(x$personal)
  expect_null(x$other_users)
  expect_null(x$shared)
})
