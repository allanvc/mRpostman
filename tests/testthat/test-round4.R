# 1.5.3: modified UTF-7 folder names, the IMAP list tokenizer, ENVELOPE and
# BODYSTRUCTURE parsers, LIST-MYRIGHTS, and CREATE (USE ...).

test_that("imap_utf7_encode()/decode() follow RFC 3501 section 5.1.3", {
  expect_identical(imap_utf7_encode("École"), "&AMk-cole")
  expect_identical(imap_utf7_encode("Q&A"), "Q&-A")
  expect_identical(imap_utf7_encode("日本語"), "&ZeVnLIqe-")
  expect_identical(imap_utf7_encode("Entwürfe"), "Entw&APw-rfe")
  expect_identical(imap_utf7_encode("INBOX"), "INBOX")
  for (nm in c("École", "Q&A", "日本語", "Entwürfe", "Sent Items", "a&b&c", "~peter/mail/台北/日本語")) {
    expect_identical(imap_utf7_decode(imap_utf7_encode(nm)), enc2utf8(nm), info = nm)
  }
  expect_identical(imap_utf7_decode("&AMk-cole"), "École")
  expect_identical(imap_utf7_decode("plain"), "plain")
})

test_that("adjust_folder_name() encodes non-ASCII names and leaves encoded ones", {
  expect_identical(mRpostman:::adjust_folder_name("École"), '"&AMk-cole"')
  expect_identical(mRpostman:::adjust_folder_name("&AMk-cole"), '"&AMk-cole"')
  expect_identical(mRpostman:::adjust_folder_name("Q&A"), '"Q&-A"')
  expect_identical(mRpostman:::adjust_folder_name("Sent Items"), '"Sent%20Items"')
})

test_that("imap_parse_list() tokenizes lists, quoted strings, literals, and NIL", {
  x <- '("a b" NIL 42 (x "y\\"z") {5}\r\nhello more)'
  l <- mRpostman:::imap_parse_list(x)
  expect_identical(l[[1]], "a b")
  expect_true(is.na(l[[2]]))
  expect_identical(l[[3]], "42")
  expect_identical(l[[4]][[2]], 'y"z')
  expect_identical(l[[5]], "hello")
  expect_identical(l[[6]], "more")
})

test_that("parse_envelope() returns analysis-ready columns", {
  x <- paste0('* 1 FETCH (ENVELOPE ("Mon, 7 May 2001 08:41:00 -0700 (PDT)" ',
              '"=?UTF-8?B?T3LDp2FtZW50bw==?=" (("Vince Kaminski" NIL "vince.kaminski" "enron.com")) ',
              '(("Vince Kaminski" NIL "vince.kaminski" "enron.com")) NIL ',
              '((NIL NIL "stephen.stock" "enron.com") (NIL NIL "beth.perlman" "enron.com")) ',
              'NIL NIL NIL "<28519439@thyme>"))\r\n')
  e <- parse_envelope(x)
  expect_identical(nrow(e), 1L)
  expect_identical(e$subject, "Orçamento")
  expect_identical(e$from, "Vince Kaminski <vince.kaminski@enron.com>")
  expect_identical(e$to, "stephen.stock@enron.com, beth.perlman@enron.com")
  expect_true(is.na(e$cc)); expect_true(is.na(e$reply_to))
  expect_identical(e$message_id, "<28519439@thyme>")
  expect_true(is.na(parse_envelope("A1 OK\r\n")$subject))
})

test_that("parse_bodystructure() numbers parts and flags attachments", {
  x <- paste0('BODYSTRUCTURE (("text" "plain" ("charset" "utf-8") NIL NIL "quoted-printable" 120 3 NIL NIL NIL NIL)',
              '("application" "pdf" ("name" "report.pdf") NIL NIL "base64" 4096 NIL ',
              '("attachment" ("filename" "report.pdf")) NIL NIL) "mixed" ("boundary" "xyz") NIL NIL NIL)')
  b <- parse_bodystructure(x)
  expect_identical(b$type, c("multipart", "text", "application"))
  expect_identical(b$part, c(NA, "1", "2"))
  expect_identical(b$charset[2], "utf-8")
  expect_identical(b$filename[3], "report.pdf")
  expect_identical(b$size[3], 4096)
  expect_identical(b$is_attachment, c(FALSE, FALSE, TRUE))
  # single-part message
  s <- parse_bodystructure('BODYSTRUCTURE ("text" "plain" ("charset" "us-ascii") NIL NIL "7bit" 50 2 NIL NIL NIL NIL)')
  expect_identical(s$part, "1"); expect_identical(nrow(s), 1L)
  # nested multipart/alternative inside mixed
  n <- parse_bodystructure(paste0('BODYSTRUCTURE ((("text" "plain" NIL NIL NIL "7bit" 10 1 NIL NIL NIL NIL)',
    '("text" "html" NIL NIL NIL "7bit" 20 1 NIL NIL NIL NIL) "alternative" NIL NIL NIL NIL)',
    '("image" "png" ("name" "a.png") NIL NIL "base64" 300 NIL ("inline" ("filename" "a.png")) NIL NIL) "mixed" NIL NIL NIL NIL)'))
  expect_identical(n$part, c(NA, "1", "1.1", "1.2", "2"))
  expect_identical(n$subtype[5], "png"); expect_true(n$is_attachment[5])
})

test_that("parse_list_extended() joins MYRIGHTS lines when asked", {
  resp <- paste0('* LIST (\\HasNoChildren) "." INBOX\r\n* MYRIGHTS INBOX lrwstipekxa\r\n',
                 '* LIST (\\HasNoChildren) "." "&AMk-cole"\r\n* MYRIGHTS "&AMk-cole" lr\r\n')
  x <- mRpostman:::parse_list_extended(resp, my_rights = TRUE)
  expect_identical(x$folder, c("INBOX", "École"))
  expect_identical(x$my_rights, c("lrwstipekxa", "lr"))
  expect_null(mRpostman:::parse_list_extended(resp)$my_rights)
})
