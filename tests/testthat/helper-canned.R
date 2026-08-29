# Canned-transport helpers: mock curl_fetch_memory so the command layer
# (request assembly, retry, error translation) is testable without a server.

canned_response <- function(headers = "", content = "", status = 0L) {
  list(headers = charToRaw(headers), content = charToRaw(content),
       status_code = status)
}

# Serve one or more canned outcomes in order; a function entry is called
# (e.g. to throw a curl-like error), any other entry is returned as is.
local_imap_transport <- function(..., .env = parent.frame()) {
  outcomes <- list(...)
  i <- 0L
  requests <- character(0)
  recorder <- new.env(parent = emptyenv())
  recorder$requests <- character(0)
  testthat::local_mocked_bindings(
    handle_setopt = function(handle, ...) {
      opts <- list(...)
      if (!is.null(opts$customrequest)) {
        recorder$requests <- c(recorder$requests, opts$customrequest)
      }
      invisible(handle)
    },
    curl_fetch_memory = function(url, handle) {
      i <<- i + 1L
      out <- outcomes[[min(i, length(outcomes))]]
      if (is.function(out)) out() else out
    },
    .package = "curl",
    .env = .env
  )
  recorder
}

# A minimal stand-in for the R6 connection, enough for imap_exec()
fake_con <- function(folder = "INBOX") {
  e <- new.env(parent = emptyenv())
  e$con_params <- list(url = "imaps://example.test", folder = folder)
  e$con_handle <- structure(list(), class = "curl_handle_stub")
  e$con_debug <- new.env(parent = emptyenv())
  e$con_debug$lines <- character(0)
  e$enabled_epochs <- NULL
  e
}
