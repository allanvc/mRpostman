test_that("sandbox_corpus() is deterministic", {
  c1 <- sandbox_corpus(n = 30)
  c2 <- sandbox_corpus(n = 30)
  expect_identical(c1, c2)
  # a different seed produces a different corpus
  c3 <- sandbox_corpus(n = 30, seed = 42)
  expect_false(identical(c1$messages, c3$messages))
})

test_that("sandbox_corpus() preserves the caller's RNG state", {
  set.seed(123)
  before <- .Random.seed
  invisible(sandbox_corpus(n = 5))
  expect_identical(before, .Random.seed)
})

test_that("sandbox_corpus() returns well-formed RFC 822 messages", {
  n <- 40
  corpus <- sandbox_corpus(n = n)

  expect_length(corpus$messages, n)
  expect_equal(nrow(corpus$info), n)

  for (i in seq_len(n)) {
    msg <- corpus$messages[[i]]
    expect_true(any(grepl("^From: ", msg)))
    expect_true(any(grepl("^To: ", msg)))
    expect_true(any(grepl("^Subject: ", msg)))
    expect_true(any(grepl("^Date: ", msg)))
    expect_true(any(grepl(sprintf("^Message-ID: <msg%04d@sandbox.local>$", i),
                          msg)))
    # a header/body separator exists
    expect_true("" %in% msg)
  }

  # Date: headers are spread over 2020 and RFC 2822-formatted in English
  dates <- corpus$info$date
  expect_true(all(grepl(
    "^(Sun|Mon|Tue|Wed|Thu|Fri|Sat), \\d{2} [A-Z][a-z]{2} 2020 \\d{2}:\\d{2}:\\d{2} \\+0000$",
    dates)))
})

test_that("sandbox_corpus() large and normal bodies are separable by size", {
  corpus <- sandbox_corpus(n = 200)
  sizes <- vapply(corpus$messages,
                  function(m) sum(nchar(m, type = "bytes")) + 2L * length(m),
                  integer(1))
  expect_true(sum(corpus$info$is_large) > 0)
  # a LARGER 8000 search must match exactly the is_large messages
  expect_true(min(sizes[corpus$info$is_large]) > 8000)
  expect_true(max(sizes[!corpus$info$is_large]) < 8000)
})

test_that("sandbox_corpus() features match the info data.frame", {
  corpus <- sandbox_corpus(n = 60)

  for (i in seq_len(60)) {
    msg <- corpus$messages[[i]]
    info <- corpus$info[i, ]

    expect_identical(any(grepl("^Content-Type: multipart/mixed", msg)),
                     info$has_attachment)
    expect_identical(any(grepl("^In-Reply-To: ", msg)), info$is_reply)
    if (info$is_reply) {
      expect_true(grepl("^Re: ", info$subject))
    }
    if (info$is_utf8_body) {
      # non-ASCII subject must be MIME encoded-word encoded in the header
      expect_true(any(grepl("^Subject: =\\?UTF-8\\?B\\?", msg)))
    }
  }
})

test_that("sandbox corpus encoded headers and bodies decode back correctly", {
  corpus <- sandbox_corpus(n = 60)

  utf8_ids <- which(corpus$info$is_utf8_body)
  expect_true(length(utf8_ids) > 0)

  for (i in utf8_ids) {
    msg <- corpus$messages[[i]]
    # the package's own decoder must recover the original subject
    subject_header <- sub("^Subject: ", "", grep("^Subject: ", msg,
                                                 value = TRUE))
    expect_identical(decode_mime_header(subject_header),
                     corpus$info$subject[i])
    # quoted-printable bodies decode back to valid UTF-8 accented text
    body_start <- which(msg == "")[1] + 1
    body <- paste(msg[body_start:length(msg)], collapse = "\n")
    decoded <- mRpostman:::decode_quoted_printable_text(body, charset = "UTF-8")
    expect_false(grepl("=C3", decoded, fixed = TRUE))
    expect_true(grepl("[áãçéêíóú]",
                      decoded))
  }
})

test_that("sandbox_corpus() validates its arguments", {
  expect_error(sandbox_corpus(n = 0))
  expect_error(sandbox_corpus(n = "a"))
  expect_error(sandbox_corpus(n = 10, seed = "a"))
})
