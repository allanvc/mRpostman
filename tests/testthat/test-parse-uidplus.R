# UIDPLUS (RFC 4315) response codes: APPENDUID and COPYUID.

test_that("expand_sequence_set() expands ranges and lists", {
  expect_identical(mRpostman:::expand_sequence_set("1:3,5,9:11"),
                   c(1L, 2L, 3L, 5L, 9L, 10L, 11L))
  expect_identical(mRpostman:::expand_sequence_set("42"), 42L)
  expect_identical(mRpostman:::expand_sequence_set(""), integer(0))
  expect_identical(mRpostman:::expand_sequence_set(NA_character_), integer(0))
})

test_that("parse_appenduid() reads the tagged OK response code", {
  resp <- "A004 OK [APPENDUID 1783866848 537] Append completed (0.010 + 0.000 secs).\r\n"
  x <- mRpostman:::parse_appenduid(resp)
  expect_identical(x, c(uidvalidity = 1783866848L, uid = 537L))
  expect_null(mRpostman:::parse_appenduid("A004 OK Append completed.\r\n"))
})

test_that("parse_copyuid() maps source UIDs to destination UIDs", {
  resp <- "A005 OK [COPYUID 38505 304,319:320 3956:3958] Done\r\n"
  x <- mRpostman:::parse_copyuid(resp)
  expect_s3_class(x, "data.frame")
  expect_identical(x$source_uid, c(304L, 319L, 320L))
  expect_identical(x$dest_uid, c(3956L, 3957L, 3958L))
  expect_identical(attr(x, "uidvalidity"), 38505L)
  expect_null(mRpostman:::parse_copyuid("A005 OK Copy completed.\r\n"))
  # mismatched set sizes are rejected rather than mis-mapped
  expect_null(mRpostman:::parse_copyuid("A5 OK [COPYUID 1 1:2 10] Done\r\n"))
})
