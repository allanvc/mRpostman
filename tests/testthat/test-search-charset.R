# issue #12: SEARCH must declare CHARSET UTF-8 (and send UTF-8 bytes) when the
# search term contains non-ASCII characters, so servers such as Gmail can match
# accented/non-Latin text. Pure-ASCII terms must be left unchanged.

test_that("search_string builds CHARSET UTF-8 only for non-ASCII terms", {
  paasuke <- intToUtf8(c(0x70, 0xE4, 0xE4, 0x73, 0x75, 0x6B, 0x65))  # Estonian

  cr_na <- mRpostman:::define_searchrequest_string(
    paasuke, where = "SUBJECT", negate = FALSE, use_uid = FALSE,
    flag = NULL, esearch = FALSE, handle = curl::new_handle())[["customrequest"]]
  expect_true(grepl("CHARSET UTF-8", cr_na, fixed = TRUE))

  cr_ascii <- mRpostman:::define_searchrequest_string(
    "hello", where = "SUBJECT", negate = FALSE, use_uid = FALSE,
    flag = NULL, esearch = FALSE, handle = curl::new_handle())[["customrequest"]]
  expect_false(grepl("CHARSET", cr_ascii, fixed = TRUE))
})

test_that("custom search adds CHARSET UTF-8 for non-ASCII requests", {
  req_na <- paste0('SUBJECT "', intToUtf8(c(0x70, 0xE4, 0xE4)), '"')
  cr <- mRpostman:::define_searchrequest_custom(
    req_na, negate = FALSE, use_uid = FALSE, esearch = FALSE,
    handle = curl::new_handle())[["customrequest"]]
  expect_true(grepl("CHARSET UTF-8", cr, fixed = TRUE))

  cr_ascii <- mRpostman:::define_searchrequest_custom(
    'SUBJECT "hello"', negate = FALSE, use_uid = FALSE, esearch = FALSE,
    handle = curl::new_handle())[["customrequest"]]
  expect_false(grepl("CHARSET", cr_ascii, fixed = TRUE))
})
