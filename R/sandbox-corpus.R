#' @title Generate a Deterministic Synthetic Mail Corpus
#' @description Generates a reproducible corpus of synthetic RFC 822 messages
#'   to be stored in a mailbox with \code{\link{populate_sandbox}}, so that the
#'   package's searching, fetching, and decoding features can be demonstrated
#'   and tested without a real mail account -- typically against the local
#'   Docker IMAP sandbox shipped in \code{system.file("docker", package =
#'   "mRpostman")}.
#' @param n Number of messages to generate. Default is \code{200}.
#' @param seed An integer used as the RNG seed. The same \code{n} and
#'   \code{seed} always produce byte-identical messages. Default is
#'   \code{3501}, after RFC 3501. The caller's RNG state is preserved.
#' @return A \code{list} with two components: \code{messages}, a list of
#'   \code{n} character vectors (the lines of each RFC 822 message); and
#'   \code{info}, a \code{data.frame} with one row per message describing the
#'   features embedded in it (\code{from}, \code{subject}, \code{date},
#'   \code{is_utf8_body}, \code{is_large}, \code{has_attachment},
#'   \code{attachment_type} (\code{"csv"}, \code{"png"}, \code{"pdf"}, or
#'   \code{NA}), \code{is_reply}, and the flags -- \code{seen},
#'   \code{flagged} -- that \code{\link{populate_sandbox}} will set after
#'   appending).
#' @details Message features are spread so that each capability of the package
#'   has matching messages to act upon: \code{Date:} headers spread over 2020
#'   (\code{SENTBEFORE}/\code{SENTSINCE}/\code{SENTON} searches); a subset of
#'   large bodies (\code{LARGER}/\code{SMALLER} searches); accented subjects in
#'   MIME encoded-words and quoted-printable bodies (header and body decoding);
#'   CSV, PNG, and one-page PDF attachments -- all generated deterministically
#'   in base R -- some with repeated filenames (attachment listing, fetching,
#'   binary base64 decoding, and filename deduplication); and reply chains via
#'   \code{In-Reply-To}/\code{References} (\code{SORT} and \code{THREAD}).
#' @family sandbox
#' @export
#' @examples
#' corpus <- sandbox_corpus(n = 5)
#' # the first synthetic message
#' cat(corpus$messages[[1]], sep = "\n")
#' # features embedded in each message
#' corpus$info
sandbox_corpus <- function(n = 200, seed = 3501) {

  assertthat::assert_that(
    is.numeric(n), length(n) == 1, is.finite(n), n >= 1,
    msg='"n" must be a single number greater than or equal to 1.')

  assertthat::assert_that(
    is.numeric(seed), length(seed) == 1, is.finite(seed),
    msg='"seed" must be a single number.')

  n <- as.integer(n)

  # preserve the caller's RNG state
  if (exists(".Random.seed", envir = globalenv())) {
    old_seed <- get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  }
  set.seed(seed)

  # feature assignment: deterministic given the seed
  sender_idx     <- sample(nrow(sandbox_people), n, replace = TRUE)
  day_offset     <- sort(sample(0:365, n, replace = TRUE)) # over 2020
  hours          <- sample(7:20, n, replace = TRUE)
  minutes        <- sample(0:59, n, replace = TRUE)
  is_utf8_body   <- stats::runif(n) < 0.15  # quoted-printable, accented
  # large bodies (> 10 kb) are drawn from the ASCII branch only, so the two
  # feature sets do not overlap and info$is_large stays truthful
  is_large       <- stats::runif(n) < 0.10 & !is_utf8_body
  has_attachment <- stats::runif(n) < 0.25
  # attachment types cycle deterministically among the flagged messages
  attachment_type <- ifelse(has_attachment,
                            c("csv", "png", "pdf")[(seq_len(n) %% 3) + 1],
                            NA_character_)
  has_cc         <- stats::runif(n) < 0.20
  is_reply       <- c(FALSE, stats::runif(n - 1) < 0.20)[seq_len(n)]
  seen           <- stats::runif(n) < 0.60
  flagged        <- stats::runif(n) < 0.10

  messages <- vector("list", n)
  subjects <- character(n)
  dates    <- character(n)

  for (i in seq_len(n)) {

    from <- sprintf("%s <%s>", sandbox_people$name[sender_idx[i]],
                    sandbox_people$email[sender_idx[i]])
    date <- sandbox_rfc2822_date(as.Date("2020-01-01") + day_offset[i],
                                 hours[i], minutes[i], (i * 7) %% 60)
    msg_id <- sprintf("<msg%04d@sandbox.local>", i)

    if (is_utf8_body[i]) {
      subject <- sample(sandbox_subjects_utf8, 1)
      n_para <- sample(2:5, 1)
      body_lines <- sample(sandbox_paragraphs_utf8, n_para, replace = TRUE)
    } else {
      subject <- sample(sandbox_subjects_ascii, 1)
      n_para <- if (is_large[i]) sample(200:400, 1) else sample(1:8, 1)
      body_lines <- sample(sandbox_paragraphs, n_para, replace = TRUE)
    }
    # blank line between paragraphs
    body_lines <- as.vector(rbind(body_lines, ""))

    headers <- c(
      sprintf("From: %s", from),
      sprintf("To: %s", sandbox_owner),
      if (has_cc[i]) sprintf("Cc: %s",
                             sandbox_people$email[(sender_idx[i] %% 8) + 1]),
      sprintf("Date: %s", date),
      sprintf("Message-ID: %s", msg_id)
    )

    # reply chains give SORT and THREAD something to work with
    if (is_reply[i]) {
      parent <- sample(i - 1, 1)
      subject <- paste("Re:", subject)
      headers <- c(headers,
                   sprintf("In-Reply-To: <msg%04d@sandbox.local>", parent),
                   sprintf("References: <msg%04d@sandbox.local>", parent))
    }
    headers <- c(headers,
                 sprintf("Subject: %s", sandbox_encode_header(subject)),
                 "MIME-Version: 1.0")

    if (has_attachment[i]) {
      boundary <- sprintf("----=_mrpostman_%04d", i)
      att <- sandbox_attachment(attachment_type[i], i)
      messages[[i]] <- c(
        headers,
        sprintf("Content-Type: multipart/mixed; boundary=\"%s\"", boundary),
        "",
        sprintf("--%s", boundary),
        "Content-Type: text/plain; charset=utf-8",
        "Content-Transfer-Encoding: quoted-printable",
        "",
        sandbox_qp_encode_lines(body_lines),
        sprintf("--%s", boundary),
        sprintf("Content-Type: %s; name=\"%s\"", att$content_type,
                att$filename),
        "Content-Transfer-Encoding: base64",
        sprintf("Content-Disposition: attachment; filename=\"%s\"",
                att$filename),
        "",
        att$b64_lines,
        sprintf("--%s--", boundary))
    } else {
      messages[[i]] <- c(
        headers,
        "Content-Type: text/plain; charset=utf-8",
        "Content-Transfer-Encoding: quoted-printable",
        "",
        sandbox_qp_encode_lines(body_lines))
    }

    subjects[i] <- subject
    dates[i] <- date

  }

  info <- data.frame(
    id             = seq_len(n),
    from           = sandbox_people$email[sender_idx],
    subject        = subjects,
    date           = dates,
    is_utf8_body   = is_utf8_body,
    is_large       = is_large,
    has_attachment = has_attachment,
    attachment_type = attachment_type,
    is_reply       = is_reply,
    seen           = seen,
    flagged        = flagged,
    stringsAsFactors = FALSE
  )

  return(list(messages = messages, info = info))

}


# ---- deterministic building blocks (internal) --------------------------------

sandbox_people <- data.frame(
  name  = c("Alice Fontes", "Bruno Keller", "Carla Mendes", "Daniel Ochoa",
            "Erika Steiner", "Felipe Duarte", "Grace Holloway", "Henrik Olsen"),
  email = c("alice.fontes@example.com", "bruno.keller@example.org",
            "carla.mendes@example.com", "daniel.ochoa@example.net",
            "erika.steiner@example.org", "felipe.duarte@example.com",
            "grace.holloway@example.net", "henrik.olsen@example.org"),
  stringsAsFactors = FALSE
)

sandbox_owner <- "Test User <testuser@sandbox.local>"

sandbox_subjects_ascii <- c(
  "Quarterly sales figures", "Meeting minutes", "Budget review",
  "Server maintenance window", "New hire onboarding", "Invoice attached",
  "Project Atlas status", "Customer feedback summary", "Travel arrangements",
  "Draft agenda for next week"
)

# non-ASCII subjects exercise the MIME encoded-word decoding
# (Portuguese, with the accented characters \uxxxx-escaped for CRAN portability)
sandbox_subjects_utf8 <- c(
  "Relat\u00f3rio de vendas", "Reuni\u00e3o de planejamento",
  "Or\u00e7amento anual", "Confer\u00eancia de imprensa"
)

sandbox_paragraphs <- c(
  "Please find below the latest numbers for this quarter.",
  "Let me know if you need any further details before the meeting.",
  "The team has reviewed the proposal and we are ready to move forward.",
  "As discussed, the deadline was moved to the end of the month.",
  "I am attaching the spreadsheet with the consolidated figures.",
  "Could you confirm your availability for a quick call tomorrow?",
  "The maintenance window is scheduled for Saturday night.",
  "Thanks for the quick turnaround on the last request.",
  "This supersedes the previous version sent last week.",
  "Feel free to forward this to anyone who might be interested."
)

sandbox_paragraphs_utf8 <- c(
  "Segue em anexo o relat\u00f3rio com os n\u00fameros consolidados.",
  "A reuni\u00e3o foi remarcada para a pr\u00f3xima ter\u00e7a-feira.",
  "Qualquer d\u00favida, estou \u00e0 disposi\u00e7\u00e3o.",
  "O or\u00e7amento precisa ser aprovado at\u00e9 sexta-feira."
)

sandbox_products <- c("widget", "gadget", "sprocket", "gizmo", "doohickey")

# RFC 2822 date, locale-independent (English day/month names by construction)
#' @noRd
sandbox_rfc2822_date <- function(date, hour, minute, second) {
  lt <- as.POSIXlt(date)
  sprintf("%s, %02d %s %d %02d:%02d:%02d +0000",
          c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")[lt$wday + 1],
          lt$mday, month.abb[lt$mon + 1], lt$year + 1900,
          hour, minute, second)
}

# RFC 2047 encoded-word for non-ASCII header values
#' @noRd
sandbox_encode_header <- function(x) {
  if (all(charToRaw(enc2utf8(x)) <= as.raw(127))) return(x)
  paste0("=?UTF-8?B?", base64enc::base64encode(charToRaw(enc2utf8(x))), "?=")
}

# line-by-line quoted-printable encoding (bodies are built from short lines)
#' @noRd
sandbox_qp_encode_lines <- function(lines) {
  vapply(lines, function(line) {
    bytes <- as.integer(charToRaw(enc2utf8(line)))
    chars <- vapply(bytes, function(b) {
      if ((b >= 33 && b <= 126 && b != 61) || b == 32) {
        rawToChar(as.raw(b))
      } else {
        sprintf("=%02X", b)
      }
    }, character(1))
    paste(chars, collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

#' @noRd
sandbox_csv_attachment <- function() {
  rows <- sprintf("%d,%s,%.2f", 1:5,
                  sample(sandbox_products, 5, replace = TRUE),
                  round(stats::runif(5) * 1000, 2))
  base64enc::base64encode(charToRaw(paste(c("id,product,amount", rows),
                                          collapse = "\r\n")))
}

# one attachment per flagged message, cycling among CSV, PNG, and PDF;
# filenames repeat across messages on purpose (dedup on saving)
#' @noRd
sandbox_attachment <- function(type, i) {
  if (type == "csv") {
    list(content_type = "text/csv",
         filename = sprintf("sales_report_%02d.csv", (i %% 7) + 1),
         b64_lines = sandbox_csv_attachment())
  } else if (type == "png") {
    list(content_type = "image/png",
         filename = sprintf("chart_%02d.png", (i %% 5) + 1),
         b64_lines = sandbox_wrap_b64(
           base64enc::base64encode(sandbox_png_bytes())))
  } else {
    list(content_type = "application/pdf",
         filename = sprintf("report_%02d.pdf", (i %% 5) + 1),
         b64_lines = sandbox_wrap_b64(
           base64enc::base64encode(sandbox_pdf_bytes())))
  }
}

# wrap a base64 string at 76 characters (RFC 2045 line-length limit)
#' @noRd
sandbox_wrap_b64 <- function(b64) {
  starts <- seq(1, nchar(b64), by = 76)
  substring(b64, starts, pmin(starts + 75L, nchar(b64)))
}

# CRC32 in the big-endian order PNG chunks need -- base R exposes no CRC32
# function, so this is the classic table-driven implementation (bitwShiftR
# treats its argument as an unsigned 32-bit pattern, i.e. a logical shift)
sandbox_crc32_table <- local({
  tab <- integer(256)
  for (i in 0:255) {
    crc <- i
    for (k in 1:8) {
      crc <- if (bitwAnd(crc, 1L) != 0L) {
        bitwXor(-306674912L, bitwShiftR(crc, 1L)) # 0xEDB88320
      } else {
        bitwShiftR(crc, 1L)
      }
    }
    tab[i + 1L] <- crc
  }
  tab
})

#' @noRd
sandbox_crc32 <- function(bytes) {
  crc <- -1L # 0xFFFFFFFF
  for (b in as.integer(bytes)) {
    idx <- bitwAnd(bitwXor(crc, b), 255L)
    crc <- bitwXor(sandbox_crc32_table[idx + 1L], bitwShiftR(crc, 8L))
  }
  writeBin(bitwXor(crc, -1L), raw(), size = 4L, endian = "big")
}

#' @noRd
sandbox_adler32 <- function(bytes) {
  a <- 1L
  b <- 0L
  for (v in as.integer(bytes)) {
    a <- (a + v) %% 65521L
    b <- (b + a) %% 65521L
  }
  as.raw(c(b %/% 256L, b %% 256L, a %/% 256L, a %% 256L))
}

# PNG's IDAT needs a zlib (RFC 1950) stream. Despite its name, R's
# memCompress(type = "gzip") emits exactly that (zlib-wrapped deflate, no
# gzip container); should some R version ever emit a real gzip container,
# the deflate payload is rewrapped as zlib
#' @noRd
sandbox_zlib <- function(bytes) {
  z <- memCompress(bytes, type = "gzip")
  if (z[1] == as.raw(0x1f) && z[2] == as.raw(0x8b)) {
    deflate <- z[11L:(length(z) - 8L)] # strip 10-byte header, 8-byte trailer
    z <- c(as.raw(c(0x78, 0x9c)), deflate, sandbox_adler32(bytes))
  }
  z
}

# a tiny deterministic 16x16 RGB gradient PNG, assembled chunk by chunk
#' @noRd
sandbox_png_bytes <- function() {
  w <- 16L
  h <- 16L
  int_be <- function(x) writeBin(as.integer(x), raw(), size = 4L, endian = "big")
  chunk <- function(type, data) {
    body <- c(charToRaw(type), data)
    c(int_be(length(data)), body, sandbox_crc32(body))
  }
  scanlines <- raw(0)
  for (y in 0:(h - 1L)) {
    px <- unlist(lapply(0:(w - 1L),
                        function(x) c(x * 16L, y * 16L, (x + y) * 8L) %% 256L))
    scanlines <- c(scanlines, as.raw(0L), as.raw(px)) # filter 0 + RGB row
  }
  ihdr <- c(int_be(w), int_be(h), as.raw(c(8L, 2L, 0L, 0L, 0L))) # 8-bit RGB
  c(as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)),
    chunk("IHDR", ihdr),
    chunk("IDAT", sandbox_zlib(scanlines)),
    chunk("IEND", raw(0)))
}

# a minimal deterministic one-page PDF (pure ASCII, offsets computed)
#' @noRd
sandbox_pdf_bytes <- function() {
  content <- "BT /F1 24 Tf 72 720 Td (mRpostman sandbox report) Tj ET"
  objs <- c(
    "<< /Type /Catalog /Pages 2 0 R >>",
    "<< /Type /Pages /Kids [3 0 R] /Count 1 >>",
    paste0("<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] ",
           "/Resources << /Font << /F1 4 0 R >> >> /Contents 5 0 R >>"),
    "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>",
    sprintf("<< /Length %d >>\nstream\n%s\nendstream",
            nchar(content), content)
  )
  header <- "%PDF-1.4\n"
  body <- sprintf("%d 0 obj\n%s\nendobj\n", seq_along(objs), objs)
  sizes <- nchar(body, type = "bytes")
  offsets <- cumsum(c(nchar(header), sizes[-length(sizes)]))
  xref_pos <- nchar(header) + sum(sizes)
  # xref entries are exactly 20 bytes each, per the PDF spec
  entries <- c("0000000000 65535 f \n", sprintf("%010d 00000 n \n", offsets))
  xref <- paste0("xref\n0 6\n", paste(entries, collapse = ""))
  trailer <- sprintf(
    "trailer\n<< /Size 6 /Root 1 0 R >>\nstartxref\n%d\n%%%%EOF\n", xref_pos)
  charToRaw(paste0(header, paste(body, collapse = ""), xref, trailer))
}
