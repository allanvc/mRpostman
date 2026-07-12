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

test_that("sandbox corpus attachments are valid CSV, PNG, and PDF payloads", {
  corpus <- sandbox_corpus(n = 90)
  info <- corpus$info

  att_types <- info$attachment_type[info$has_attachment]
  expect_true(all(c("csv", "png", "pdf") %in% att_types))
  expect_true(all(is.na(info$attachment_type[!info$has_attachment])))

  # the base64 payload sits between the last blank line and the closing
  # boundary
  extract_payload <- function(msg) {
    blanks <- which(msg == "")
    base64enc::base64decode(paste(msg[(max(blanks) + 1):(length(msg) - 1)],
                                  collapse = ""))
  }

  for (i in which(info$has_attachment)) {
    msg <- corpus$messages[[i]]
    payload <- extract_payload(msg)

    if (info$attachment_type[i] == "csv") {
      expect_true(any(grepl('name="sales_report_\\d+\\.csv"', msg)))
      expect_identical(rawToChar(payload[1:17]), "id,product,amount")
    } else if (info$attachment_type[i] == "png") {
      expect_true(any(grepl("^Content-Type: image/png", msg)))
      expect_true(any(grepl('name="chart_\\d+\\.png"', msg)))
      # PNG signature and IHDR dimensions (16 x 16)
      expect_identical(payload[1:8],
                       as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a,
                                0x0a)))
      expect_identical(payload[17:24],
                       as.raw(c(0, 0, 0, 16, 0, 0, 0, 16)))
    } else {
      expect_true(any(grepl("^Content-Type: application/pdf", msg)))
      expect_true(any(grepl('name="report_\\d+\\.pdf"', msg)))
      txt <- rawToChar(payload)
      expect_true(startsWith(txt, "%PDF-1.4"))
      expect_true(grepl("%%EOF", txt))
      expect_true(grepl("/Type /Catalog", txt, fixed = TRUE))
    }

    # RFC 5322 hard limit on line length
    expect_true(all(nchar(msg) <= 998))
  }
})

test_that("sandbox PNG chunk CRCs are correct (table-driven CRC32)", {
  # classic CRC32 check value
  expect_identical(
    mRpostman:::sandbox_crc32(charToRaw("123456789")),
    as.raw(c(0xcb, 0xf4, 0x39, 0x26)))
})

test_that("sandbox_corpus() validates its arguments", {
  expect_error(sandbox_corpus(n = 0))
  expect_error(sandbox_corpus(n = "a"))
  expect_error(sandbox_corpus(n = 10, seed = "a"))
})
