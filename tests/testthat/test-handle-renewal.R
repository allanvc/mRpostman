# curl's multi interface (<= 7.1.0) keeps the response headers of every
# transfer on the handle, so a long run of commands on one connection copies
# an ever larger buffer out of libcurl at every command. curl_perform() renews
# the handle (reset + options applied again) once the buffer passes a limit.
# Exercised offline with file:// transfers: no network, real libcurl.

local_file_url <- function() {
  tf <- tempfile("mrp-renew-")   # under tempdir(), removed when the session ends
  writeLines(strrep("x", 2000), tf)
  paste0("file://", if (.Platform$OS.type == "windows") "/" else "",
         normalizePath(tf, winslash = "/"))
}

# a stand-in for the connection object, as configure_imap() would build it
renewable_con <- function(url) {
  dbg <- new.env(parent = emptyenv())
  dbg$verbose <- FALSE; dbg$lines <- character(0); dbg$epoch <- 0L
  self <- new.env(parent = emptyenv())
  self$con_handle <- curl::new_handle()
  self$con_params <- list(url = url)
  self$con_debug <- dbg
  self$con_opts <- new.env(parent = emptyenv())
  self$con_opts$params <- list(useragent = "mRpostman-renewal-test")
  self$con_opts$debugfunction <- mRpostman:::make_debug_function(dbg)
  self$con_opts$renewals <- 0L
  self
}

test_that("the handle is renewed once the accumulated buffer passes the limit", {
  skip_if_not(mRpostman:::multi_headers_accumulate(),
              "this curl clears the buffer between transfers itself")
  url <- local_file_url()
  self <- renewable_con(url)
  old <- options(mRpostman.handle_buffer_limit = 300); on.exit(options(old), add = TRUE)

  real_setopt <- curl::handle_setopt
  applied <- list()
  testthat::local_mocked_bindings(
    handle_setopt = function(handle, ...) {
      applied[[length(applied) + 1L]] <<- names(list(...))
      real_setopt(handle, ...)
    },
    .package = "curl")

  sizes <- integer(0)
  for (i in 1:8) {
    out <- mRpostman:::curl_perform(url, self$con_handle, self)
    sizes <- c(sizes, length(out$headers))
  }
  # every result carries the headers of its own transfer only
  expect_gt(sizes[1], 0L)
  expect_true(all(sizes == sizes[1]))
  # the buffer was emptied at least once and never grew past limit + one reply
  expect_gt(self$con_opts$renewals, 0L)
  expect_lt(length(curl::handle_data(self$con_handle)$headers), 300 + sizes[1])
  # a renewal applies the recorded options and the debug callback again
  expect_true(any(vapply(applied, function(x) "useragent" %in% x, logical(1))))
  expect_true(any(vapply(applied, function(x) "debugfunction" %in% x, logical(1))))
})

test_that("below the limit the handle is left alone", {
  url <- local_file_url()
  self <- renewable_con(url)
  old <- options(mRpostman.handle_buffer_limit = 1e9); on.exit(options(old), add = TRUE)
  for (i in 1:4) mRpostman:::curl_perform(url, self$con_handle, self)
  expect_identical(self$con_opts$renewals, 0L)
})

test_that("a connection without the option record is never renewed", {
  url <- local_file_url()
  self <- renewable_con(url)
  self$con_opts <- NULL
  old <- options(mRpostman.handle_buffer_limit = 1); on.exit(options(old), add = TRUE)
  sizes <- integer(0)
  for (i in 1:3) {
    out <- mRpostman:::curl_perform(url, self$con_handle, self)
    sizes <- c(sizes, length(out$headers))
  }
  expect_true(all(sizes == sizes[1]))   # the slice logic still applies
  expect_false(mRpostman:::renew_handle(self, self$con_handle))
})

test_that("configure_imap() records the handle options and the resets keep them in sync", {
  con <- configure_imap(url = "imap://imap.example.test", username = "u",
                        password = "p", use_ssl = FALSE, timeout_ms = 1234)
  p <- con$con_opts$params
  expect_identical(p$username, "u")
  expect_identical(p$password, "p")
  expect_identical(p$use_ssl, 0L)
  expect_identical(p$timeout_ms, 1234)
  expect_identical(con$con_opts$renewals, 0L)
  expect_true(is.function(con$con_opts$debugfunction))

  con$reset_use_ssl(TRUE)
  expect_identical(con$con_opts$params$use_ssl, 3L)
  con$reset_xoauth2_bearer("tok")
  expect_identical(con$con_opts$params$xoauth2_bearer, "tok")
  expect_null(con$con_opts$params$password)
  con$reset_password("p2")
  expect_identical(con$con_opts$params$password, "p2")
  expect_null(con$con_opts$params$xoauth2_bearer)
  con$reset_timeout_ms(99)
  expect_identical(con$con_opts$params$timeout_ms, 99)

  con$disconnect()
  expect_null(con$con_opts)
})

test_that("the limit option falls back to the default when malformed", {
  old <- options(mRpostman.handle_buffer_limit = "big"); on.exit(options(old), add = TRUE)
  expect_identical(mRpostman:::handle_buffer_limit(), 65536)
  options(mRpostman.handle_buffer_limit = NULL)
  expect_identical(mRpostman:::handle_buffer_limit(), 65536)
})
