# 1.5.4: BADCHARSET fallback requests and MIME part decoding.

test_that("charset_fallback_requests() rewrites the request in accepted charsets", {
  req <- "SEARCH CHARSET UTF-8 (SUBJECT Reuni\u00e3o)"
  alts <- mRpostman:::charset_fallback_requests(req, "NO [BADCHARSET (ISO-8859-1 KOI8-R)] Unknown charset")
  expect_identical(length(alts), 1L)                               # only ISO-8859-1 can hold "ã"
  expect_true(grepl("^SEARCH CHARSET ISO-8859-1 ", alts[1], useBytes = TRUE))
  expect_true(as.raw(0xe3) %in% charToRaw(alts[1]))                # "ã" as its single Latin-1 byte
  expect_false(as.raw(0xc3) %in% charToRaw(alts[1]))               # ... not as UTF-8
  # no charsets listed by the server: ISO-8859-1 is tried, US-ASCII is not representable
  alts2 <- mRpostman:::charset_fallback_requests(req, "NO [BADCHARSET] Unknown charset")
  expect_identical(length(alts2), 1L)
  # an ASCII request without a CHARSET clause gets one for every candidate
  alts3 <- mRpostman:::charset_fallback_requests("SEARCH (SUBJECT test)", "NO [BADCHARSET]")
  expect_true(all(grepl("^SEARCH CHARSET ", alts3, useBytes = TRUE)))
  expect_identical(length(alts3), 2L)
})

test_that("qp_decode_raw() and decode_part_raw() decode transfer encodings", {
  expect_identical(rawToChar(mRpostman:::qp_decode_raw("Reuni=C3=A3o=\r\n de=20equipe")),
                   "Reuni\u00e3o de equipe")
  expect_identical(mRpostman:::qp_decode_raw("=00=FF"), as.raw(c(0x00, 0xff)))
  png_sig <- as.raw(c(0x89, 0x50, 0x4e, 0x47))
  expect_identical(mRpostman:::decode_part_raw(base64enc::base64encode(png_sig), "base64"), png_sig)
  expect_identical(mRpostman:::decode_part_raw("abc", "7bit"), charToRaw("abc"))
  expect_identical(mRpostman:::decode_part_raw("abc", NA), charToRaw("abc"))
})

test_that("size and time criteria never use scientific notation", {
  expect_identical(larger_than(5e6), "(LARGER 5000000)")
  expect_identical(smaller_than(1e5, negate = TRUE), "(NOT (SMALLER 100000))")
  expect_identical(younger_than(1e6), "(YOUNGER 1000000)")
  expect_identical(older_than(2e7), "(OLDER 20000000)")
})
