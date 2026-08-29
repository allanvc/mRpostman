# 2.3.0: the real MIME parser behind the attachment helpers, against a
# torture corpus (offline), plus a live round trip on the sandbox.

b64 <- function(x) base64enc::base64encode(x)
wrap76 <- function(x) paste(regmatches(x, gregexpr(".{1,76}", x))[[1]], collapse = "\r\n")
payload_all <- as.raw(0:255)                        # every byte value
payload_png <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 1:8))

torture_simple <- paste0(
  "From: a@b.c\r\nTo: d@e.f\r\nSubject: t1\r\nMIME-Version: 1.0\r\n",
  "Content-Type: multipart/mixed; boundary=\"BB1\"\r\n\r\n",
  "--BB1\r\nContent-Type: text/plain\r\n\r\nhello\r\n",
  "--BB1\r\nContent-Type: application/pdf; name=\"report.pdf\"\r\n",
  "Content-Disposition: attachment; filename=\"report.pdf\"\r\n",
  "Content-Transfer-Encoding: base64\r\n\r\n", wrap76(b64(payload_all)), "\r\n",
  "--BB1--\r\n")

torture_nested <- paste0(
  "Subject: t2\r\nMIME-Version: 1.0\r\n",
  "Content-Type: multipart/mixed; boundary=\"OUTER\"\r\n\r\n",
  "--OUTER\r\nContent-Type: multipart/alternative; boundary=\"INNER\"\r\n\r\n",
  "--INNER\r\nContent-Type: text/plain\r\n\r\nplain body\r\n",
  "--INNER\r\nContent-Type: multipart/related; boundary=\"DEEP\"\r\n\r\n",
  "--DEEP\r\nContent-Type: text/html\r\n\r\n<p>html</p>\r\n",
  "--DEEP\r\nContent-Type: image/png; name=\"logo.png\"\r\n",
  "Content-Disposition: inline; filename=\"logo.png\"\r\n",
  "Content-Transfer-Encoding: base64\r\n\r\n", wrap76(b64(payload_png)), "\r\n",
  "--DEEP--\r\n",
  "--INNER--\r\n",
  "--OUTER\r\nContent-Type: text/csv; name=\"data.csv\"\r\n",
  "Content-Disposition: attachment; filename=\"data.csv\"\r\n",
  "Content-Transfer-Encoding: base64\r\n\r\n", b64(charToRaw("a,b\r\n1,2\r\n")), "\r\n",
  "--OUTER--\r\n")

torture_qp <- paste0(
  "Subject: t3\r\nMIME-Version: 1.0\r\n",
  "Content-Type: multipart/mixed; boundary=\"QQ\"\r\n\r\n",
  "--QQ\r\nContent-Type: text/plain\r\n\r\nbody\r\n",
  "--QQ\r\nContent-Type: text/plain; name=\"nota.txt\"\r\n",
  "Content-Disposition: attachment; filename=\"nota.txt\"\r\n",
  "Content-Transfer-Encoding: quoted-printable\r\n\r\n",
  "Relat=C3=B3rio at=C3=A9 sex=\r\nta.\r\n",
  "--QQ--\r\n")

torture_names <- paste0(
  "Subject: t4\r\nMIME-Version: 1.0\r\n",
  "Content-Type: multipart/mixed; boundary=\"NN\"\r\n\r\n",
  "--NN\r\nContent-Type: text/plain\r\n\r\nbody\r\n",
  "--NN\r\nContent-Type: application/octet-stream\r\n",
  "Content-Disposition: attachment; filename*=UTF-8''relat%C3%B3rio%20--%20final.bin\r\n",
  "Content-Transfer-Encoding: base64\r\n\r\n", b64(as.raw(1:32)), "\r\n",
  "--NN\r\nContent-Type: application/octet-stream\r\n",
  "Content-Disposition: attachment;\r\n filename==?UTF-8?Q?an=C3=A1lise=2Ebin?=\r\n",
  "Content-Transfer-Encoding: base64\r\n\r\n", b64(as.raw(33:64)), "\r\n",
  "--NN\r\nContent-Type: application/octet-stream; name=unquoted.bin\r\n",
  "Content-Transfer-Encoding: base64\r\n\r\n", b64(as.raw(65:96)), "\r\n",
  "--NN--\r\n")

torture_lf <- gsub("\r\n", "\n", torture_simple, fixed = TRUE)

test_that("split_mime_parts() reassembles payloads byte-identically", {
  p <- mRpostman:::split_mime_parts(torture_simple)
  a <- mRpostman:::mime_attachment_parts(p, "both")
  expect_identical(a$filename, "report.pdf")
  expect_identical(a$payload[[1]], payload_all)

  p <- mRpostman:::split_mime_parts(torture_nested)
  a <- mRpostman:::mime_attachment_parts(p, "both")
  expect_identical(a$filename, c("logo.png", "data.csv"))
  expect_identical(a$disposition, c("inline", "attachment"))
  expect_identical(a$payload[[1]], payload_png)
  expect_identical(rawToChar(a$payload[[2]]), "a,b\r\n1,2\r\n")
  expect_identical(nrow(mRpostman:::mime_attachment_parts(p, "attachment")), 1L)

  p <- mRpostman:::split_mime_parts(torture_qp)
  a <- mRpostman:::mime_attachment_parts(p, "both")
  expect_identical(rawToChar(a$payload[[1]]), "Relatório até sexta.")

  p <- mRpostman:::split_mime_parts(torture_lf)
  a <- mRpostman:::mime_attachment_parts(p, "both")
  expect_identical(a$payload[[1]], payload_all)
})

test_that("filenames survive RFC 2231, encoded words, unquoted, and dashes", {
  a <- mRpostman:::mime_attachment_parts(mRpostman:::split_mime_parts(torture_names), "both")
  expect_identical(a$filename[1], "relatório -- final.bin")
  expect_identical(a$filename[2], "análise.bin")
  expect_identical(a$filename[3], "unquoted.bin")
  expect_identical(a$payload[[1]], as.raw(1:32))
  expect_identical(a$payload[[2]], as.raw(33:64))
  expect_identical(a$payload[[3]], as.raw(65:96))
})

test_that("list_attachments() and get_attachments() run on the torture corpus", {
  msgs <- list(torture_nested, torture_names)
  names(msgs) <- c("bodyUID11", "bodyUID12")
  la <- list_attachments(msgs)
  expect_identical(la[["bodyUID11"]]$filename, c("logo.png", "data.csv"))
  expect_identical(nrow(la[["bodyUID12"]]), 3L)

  con <- configure_imap(url = "imap://localhost:1430", username = "att",
                        password = "x", use_ssl = FALSE)
  con$con_params$folder <- "INBOX"
  tmp <- file.path(tempdir(), paste0("attx", as.integer(runif(1, 1, 1e6))))
  expect_true(get_attachments_int <- mRpostman:::get_attachments_int(
    con, msgs, content_disposition = "both", override = TRUE, mute = TRUE,
    as_is = FALSE, local_dir = tmp))
  f <- file.path(tmp, "att", "INBOX", "UID11", "logo.png")
  expect_true(file.exists(f))
  expect_identical(readBin(f, "raw", 1e4), payload_png)
  f2 <- file.path(tmp, "att", "INBOX", "UID12", "relatório -- final.bin")
  expect_true(file.exists(f2))
  expect_identical(readBin(f2, "raw", 1e4), as.raw(1:32))
  unlink(tmp, recursive = TRUE)
})


test_that("the whole attachment family round-trips on the sandbox", {
  skip_if_not(att_sandbox_up(), "Docker sandbox not reachable on localhost:1430")
  con <- configure_imap(url = "imap://localhost:1430",
                        username = paste0("att", format(Sys.time(), "%H%M%S")),
                        password = "sandbox", use_ssl = FALSE)
  for (m in list(torture_simple, torture_nested, torture_qp, torture_names)) {
    con$append_msg(m, folder = "INBOX", mute = TRUE)
  }
  con$select_folder("INBOX", mute = TRUE)

  # fetch_attachment_parts: in-memory payloads, byte-identical
  ap <- con$fetch_attachment_parts(msg_id = 2, local_dir = NULL)
  expect_identical(sort(ap$filename), sort(c("logo.png", "data.csv")))
  expect_identical(ap$content[[which(ap$filename == "logo.png")]], payload_png)

  # fetch_attachments (now BODYSTRUCTURE-guided): files on disk
  tmp <- file.path(tempdir(), paste0("atty", as.integer(runif(1, 1, 1e6))))
  con$fetch_attachments(msg_id = c(1, 2, 4), local_dir = tmp, mute = TRUE, override = TRUE)
  base <- list.dirs(tmp, recursive = FALSE)
  f1 <- list.files(tmp, pattern = "^report.pdf$", recursive = TRUE, full.names = TRUE)
  expect_length(f1, 1)
  expect_identical(readBin(f1, "raw", 1e4), payload_all)
  f2 <- list.files(tmp, pattern = "logo.png", recursive = TRUE, full.names = TRUE)
  expect_identical(readBin(f2[1], "raw", 1e4), payload_png)

  # cross-check with the server-side decode (BINARY)
  bs <- con$fetch_bodystructure(msg_id = 1)
  part <- bs$part[which(bs$filename == "report.pdf")]
  expect_identical(con$fetch_binary(msg_id = 1, part = part)[[1]], payload_all)

  # fetch_body + get_attachments legacy combo
  body <- con$fetch_body(msg_id = 3)
  tmp2 <- file.path(tempdir(), paste0("attz", as.integer(runif(1, 1, 1e6))))
  con$get_attachments(body, mute = TRUE, local_dir = tmp2)
  f3 <- list.files(tmp2, pattern = "nota.txt", recursive = TRUE, full.names = TRUE)
  expect_length(f3, 1)
  expect_identical(rawToChar(readBin(f3, "raw", 1e4)), "Relatório até sexta.")

  # listing paths
  al <- con$fetch_attachments_list(msg_id = 2)
  expect_true("data.csv" %in% al[[1]]$filename)
  unlink(c(tmp, tmp2), recursive = TRUE)
  con$disconnect()
})
