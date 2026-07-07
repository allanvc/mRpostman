# issue #13: disconnect() should release the connection handle so it can no
# longer be used. configure_imap() only builds the handle (it does not open a
# connection until the first command), so this test is offline-safe.

test_that("disconnect() releases the handle and invalidates the connection", {
  con <- configure_imap(url = "imaps://imap.example.com",
                        username = "user@example.com", password = "dummy")
  expect_false(is.null(con$con_handle))

  expect_true(con$disconnect())
  expect_null(con$con_handle)
  expect_true(is.na(con$con_params$folder))
})
