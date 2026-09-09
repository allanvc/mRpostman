# 3.0.0: use_ssl = TRUE means TLS is required. On an imap:// URL the server
# must offer STARTTLS; there is no fallback to a plaintext login, not even
# through a connection libcurl cached earlier under a weaker setting.

test_that("ssl_level() maps use_ssl to the strict CURLOPT_USE_SSL value", {
  expect_identical(mRpostman:::ssl_level(TRUE), 3L)   # CURLUSESSL_ALL
  expect_identical(mRpostman:::ssl_level(FALSE), 0L)  # CURLUSESSL_NONE
})

test_that("use_ssl = TRUE on a plain imap:// server is refused, never a plaintext login (sandbox)", {
  skip_if_not(sandbox_up(), "Docker sandbox not reachable on localhost:1430")
  con <- configure_imap(url = "imap://localhost:1430", username = "rawtest",
                        password = "sandbox", use_ssl = TRUE)
  expect_error(con$list_mail_folders(retries = 0), "does not offer STARTTLS")
  expect_error(con$list_mail_folders(), "does not offer STARTTLS")   # with retries
})

test_that("a plaintext connection cached by another object is never reused for a TLS one (sandbox)", {
  skip_if_not(sandbox_up(), "Docker sandbox not reachable on localhost:1430")
  plain <- configure_imap(url = "imap://localhost:1430", username = "rawtest",
                          password = "sandbox", use_ssl = FALSE)
  expect_type(plain$list_mail_folders()$root, "character")
  plain$disconnect()
  # same server and credentials: libcurl would match the cached connection
  strict <- configure_imap(url = "imap://localhost:1430", username = "rawtest",
                           password = "sandbox", use_ssl = TRUE)
  expect_error(strict$list_mail_folders(retries = 0), "does not offer STARTTLS")
})

test_that("reset_use_ssl(TRUE) drops the connections opened in plaintext (sandbox)", {
  skip_if_not(sandbox_up(), "Docker sandbox not reachable on localhost:1430")
  con <- configure_imap(url = "imap://localhost:1430", username = "rawtest",
                        password = "sandbox", use_ssl = FALSE)
  expect_type(con$list_mail_folders()$root, "character")
  con$reset_use_ssl(TRUE)
  expect_error(con$list_mail_folders(retries = 0), "does not offer STARTTLS")
  con$reset_use_ssl(FALSE)
  expect_type(con$list_mail_folders()$root, "character")
  con$disconnect()
})
