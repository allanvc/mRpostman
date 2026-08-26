test_that("literal_size() reads the literal marker of a FETCH response", {
  expect_equal(literal_size("* 263 FETCH (BODY[HEADER]<0> {60000}\r\nMessage-ID: x\r\n"), 60000L)
  expect_equal(literal_size("* 5 FETCH (UID 5 BODY[TEXT] {1234}\r\nhello\r\n)\r\nA3 OK Fetch completed.\r\n"), 1234L)
  expect_equal(literal_size("* 7 FETCH (BODY[HEADER]<120000> {0}\r\n)\r\nA9 OK Fetch completed.\r\n"), 0L)
  expect_true(is.na(literal_size("* 7 FETCH (RFC822.SIZE 94799)\r\nA9 OK Fetch completed.\r\n")))
  expect_true(is.na(literal_size("")))
})

test_that("cleaned partial slices concatenate to the full part", {
  full <- paste0("Subject: a\r\nTo: ", paste(rep("x", 200), collapse = ""), "\r\n\r\n")
  n <- nchar(full); chunk <- 100L
  slices <- lapply(seq(0, n - 1, by = chunk), function(start) {
    piece <- substr(full, start + 1, min(start + chunk, n))
    sprintf("* 1 FETCH (BODY[HEADER]<%d> {%d}\r\n%s)\r\nA1 OK Fetch completed (0.001 + 0.000 secs).\r\n",
            start, nchar(piece), piece)
  })
  sizes <- vapply(slices, literal_size, integer(1))
  expect_equal(sum(sizes), n)
  expect_equal(paste0(vapply(slices, clean_fetch_results, character(1)), collapse = ""), full)
})

test_that("fetch_in_chunks() declines requests it cannot slice", {
  # declined before any server interaction, so a dummy connection suffices
  expect_null(fetch_in_chunks(NULL, "FETCH 1 (RFC822.SIZE)"))
  expect_null(fetch_in_chunks(NULL, "FETCH 1 BODY.PEEK[HEADER]<0.500>"))
})
