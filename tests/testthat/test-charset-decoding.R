# Charset-aware MIME (RFC 2047) header and body decoding.
# These tests are pure (no server) and skip a charset when the platform's iconv
# cannot produce it, so they never fail spuriously on CRAN machines.
# Non-ASCII values are built from code points via intToUtf8() to keep this
# source file pure ASCII and portable.

u <- function(...) intToUtf8(c(...))
same_utf8 <- function(a, b) identical(charToRaw(enc2utf8(a)), charToRaw(enc2utf8(b)))

# UTF-8 bytes of the text re-encoded into `cs`; NULL when iconv can't represent it
make_bytes <- function(txt, cs) {
  raw <- tryCatch(iconv(txt, from = "UTF-8", to = cs, toRaw = TRUE)[[1]],
                  error = function(e) NULL)
  if (is.null(raw) || anyNA(raw) || length(raw) == 0) return(NULL)
  raw
}

charset_cases <- list(
  list(cs = "ISO-8859-1",   txt = u(0x63,0x61,0x66,0xE9)),                  # cafe
  list(cs = "UTF-8",        txt = u(0x63,0x61,0x66,0xE9)),
  list(cs = "WINDOWS-1251", txt = u(0x41F,0x440,0x438,0x432,0x435,0x442)),  # Privet
  list(cs = "KOI8-R",       txt = u(0x41C,0x438,0x440)),                    # Mir
  list(cs = "ISO-8859-2",   txt = u(0x5A,0x61,0x17C,0xF3,0x142,0x107)),     # Zazolc
  list(cs = "BIG5",         txt = u(0x6E2C,0x8A66)),                        # Chinese
  list(cs = "SHIFT_JIS",    txt = u(0x30C6,0x30B9,0x30C8)),                 # Japanese
  list(cs = "ISO-8859-9",   txt = u(0xE7,0xF6,0x11F,0xFC))                  # Turkish
)

test_that("decode_mime_header() honors the declared charset for B and Q encodings", {
  tested <- 0L
  for (cse in charset_cases) {
    raw <- make_bytes(cse$txt, cse$cs)
    if (is.null(raw)) next
    tested <- tested + 1L
    b64 <- base64enc::base64encode(raw)
    qp  <- paste0(sprintf("=%02X", as.integer(raw)), collapse = "")
    expect_true(same_utf8(decode_mime_header(sprintf("=?%s?B?%s?=", cse$cs, b64)), cse$txt),
                info = paste(cse$cs, "base64"))
    expect_true(same_utf8(decode_mime_header(sprintf("=?%s?Q?%s?=", cse$cs, qp)), cse$txt),
                info = paste(cse$cs, "quoted-printable"))
  }
  expect_gt(tested, 0L)  # at least ISO-8859-1/UTF-8 must be available anywhere
})

test_that("decode_mime_header() still decodes the originally documented examples", {
  expect_true(same_utf8(decode_mime_header("=?iso-8859-1?Q?DIDEC_Capacita=E7=E3o?="),
                        paste0("DIDEC Capacita", u(0xE7, 0xE3), "o")))  # Capacitacao
  expect_true(same_utf8(decode_mime_header("=?utf-8?B?Sk9BTkEgRlVTQ08gTE9CTyBubyBUZWFtcw==?="),
                        "JOANA FUSCO LOBO no Teams"))
})

test_that("message-body decoders honor the declared charset", {
  privet <- u(0x41F,0x440,0x438,0x432,0x435,0x442)  # Privet
  raw <- make_bytes(privet, "WINDOWS-1251")
  skip_if(is.null(raw), "iconv without WINDOWS-1251 support")
  qp <- paste0(sprintf("=%02X", as.integer(raw)), collapse = "")
  expect_true(same_utf8(
    mRpostman:::decode_quoted_printable_text(qp, charset = "WINDOWS-1251"),
    privet))

  privet_bang <- paste0(privet, "!")               # padded base64
  raw7 <- make_bytes(privet_bang, "WINDOWS-1251")
  skip_if(is.null(raw7), "iconv without WINDOWS-1251 support")
  expect_true(same_utf8(
    mRpostman:::decode_mime_text(base64enc::base64encode(raw7), charset = "WINDOWS-1251"),
    privet_bang))
})

test_that("apply_charset() falls back to the input for unknown/empty charsets", {
  expect_identical(mRpostman:::apply_charset("abc", NULL), "abc")
  expect_identical(mRpostman:::apply_charset("abc", ""), "abc")
  expect_identical(mRpostman:::apply_charset("abc", "NOT-A-REAL-CHARSET-XYZ"), "abc")
})
