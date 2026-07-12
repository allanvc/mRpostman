test_that("clean_msg_text() copes with invalid-encoding bytes", {
  # 8-bit garbage as found in old real-world corpora (undeclared legacy
  # charset / binary fragment); regex operations error on such strings
  bad <- rawToChar(as.raw(c(0x86, 0xdb, 0x69, 0xff, 0xfc, 0x30, 0xc3, 0x74)))

  out <- NULL
  expect_no_error(out <- clean_msg_text(list(bad)))
  expect_true(all(validEnc(unlist(out))))
})

test_that("clean_msg_text() copes with pseudo-base64 decoding to binary", {
  # the base64 heuristic can decode arbitrary text into binary containing
  # embedded NULs, on which rawToChar() errors; must fall back gracefully
  bad <- rawToChar(as.raw(c(0x41, 0x41, 0x41, 0x41, 0x86, 0xdb, 0x69, 0xff,
                            0xfc, 0x30, 0xc3, 0x66, 0xe5, 0xb9)))

  out <- NULL
  expect_no_error(out <- clean_msg_text(list(bad)))
  expect_true(all(validEnc(unlist(out))))
})

test_that("clean_msg_text() still decodes ordinary quoted-printable text", {
  qp <- "Content-Type: text/plain; charset=utf-8\r\nContent-Transfer-Encoding: quoted-printable\r\n\r\nQualquer d=C3=BAvida, estou =C3=A0 disposi=C3=A7=C3=A3o."
  out <- clean_msg_text(list(qp))
  expect_true(grepl("dúvida", out[[1]]))
  expect_true(grepl("disposição", out[[1]]))
})
