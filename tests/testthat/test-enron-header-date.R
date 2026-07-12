test_that(".enron_header_date() parses both Date: header variants", {
  with_weekday <- tempfile(fileext = ".txt")
  writeLines(c("Message-ID: <1.a@example.com>",
               "Date: Mon, 14 May 2001 16:39:00 -0700 (PDT)",
               "From: someone@example.com",
               "",
               "Body."), with_weekday)

  without_weekday <- tempfile(fileext = ".txt")
  writeLines(c("Message-ID: <2.b@example.com>",
               "Date: 3 Dec 2001 10:00:00 -0800",
               "From: someone@example.com",
               "",
               "Body."), without_weekday)

  expect_identical(mRpostman:::.enron_header_date(with_weekday), "2001-05-14")
  expect_identical(mRpostman:::.enron_header_date(without_weekday),
                   "2001-12-03")

  unlink(c(with_weekday, without_weekday))
})

test_that(".enron_header_date() copes with invalid 8-bit bytes in headers", {
  # real-world corpora contain headers with bytes that are invalid in the
  # session encoding; matching must be byte-wise and warning-free
  eightbit <- tempfile(fileext = ".txt")
  con <- file(eightbit, "wb")
  writeBin(c(charToRaw("Subject: Conferencia en Espa"), as.raw(0xf1),
             charToRaw("a\nDate: Fri, 17 Aug 2001 11:04:47 -0700 (PDT)\n\nBody.")),
           con)
  close(con)

  expect_no_warning(
    expect_identical(mRpostman:::.enron_header_date(eightbit), "2001-08-17"))

  unlink(eightbit)
})

test_that(".enron_header_date() returns NA when there is no Date header", {
  no_date <- tempfile(fileext = ".txt")
  writeLines(c("Message-ID: <3.c@example.com>",
               "From: someone@example.com",
               "",
               "Body."), no_date)

  expect_identical(mRpostman:::.enron_header_date(no_date), NA_character_)

  unlink(no_date)
})

test_that(".enron_header_date() is not fooled by a Date: line in the body", {
  # only a real header line (start of file or after a newline, before the
  # blank separator would even matter) should match; a file whose first
  # Date: occurrence is parseable must win
  body_date <- tempfile(fileext = ".txt")
  writeLines(c("Message-ID: <4.d@example.com>",
               "Date: Tue, 1 Jan 2002 08:00:00 -0800 (PST)",
               "From: someone@example.com",
               "",
               "Date: 25 Dec 2001 09:00:00 -0800"), body_date)

  expect_identical(mRpostman:::.enron_header_date(body_date), "2002-01-01")

  unlink(body_date)
})
