#' Fetch a message part in partial (chunked) requests
#'
#' Fallback used by \code{execute_fetch_loop()} when libcurl refuses a single
#' \code{FETCH} response because the literal is too large for its internal
#' buffer (libcurl >= 8.x, error \code{CURLE_TOO_LARGE}, "A value or data
#' field grew larger than allowed", observed for literals above roughly
#' 90 kB). The same request is re-issued with IMAP partial specifiers
#' (\code{<start.count>}, RFC 3501 section 6.4.5) in \code{chunk_size} slices,
#' each slice is cleaned with \code{clean_fetch_results()}, and the cleaned
#' slices are concatenated.
#'
#' @param self The R6 connection object.
#' @param fetch_request The complete \code{FETCH} custom request (without a
#'   partial specifier).
#' @param metadata_attribute Passed on to \code{clean_fetch_results()}.
#' @param chunk_size Number of bytes requested per slice.
#' @return The cleaned message text, or \code{NULL} when the request cannot be
#'   sliced (it already carries a partial specifier, or it fetches no body
#'   literal) or when a slice fails.
#' @noRd
fetch_in_chunks <- function(self, fetch_request, metadata_attribute = NULL,
                            chunk_size = 64000L) {

  # only BODY[...] literals can be sliced; a user-supplied partial is kept as is
  if (!grepl("BODY", fetch_request, ignore.case = TRUE) ||
      grepl("<\\d+\\.\\d+>\\s*$", fetch_request)) {
    return(NULL)
  }

  h <- self$con_handle
  url <- self$con_params$url

  # libcurl drops the connection on CURLE_TOO_LARGE; the reconnected session
  # has no mailbox selected, so the folder must be selected again first
  select_folder_int(self, name = self$con_params$folder, mute = TRUE, retries = 0)

  pieces <- character(0)
  start <- 0L

  repeat {
    curl::handle_setopt(
      handle = h,
      customrequest = sprintf("%s<%d.%d>", fetch_request, start, chunk_size))

    response <- tryCatch({
      curl::curl_fetch_memory(url, handle = h)
    }, error = function(e) NULL)

    if (is.null(response)) {
      return(NULL)
    }

    raw_text <- rawToChar(response$headers)
    n <- literal_size(raw_text)

    if (is.na(n) || n == 0L) { # past the end of the part
      break
    }

    pieces <- c(pieces, clean_fetch_results(raw_text, metadata_attribute))

    if (n < chunk_size) { # last slice
      break
    }
    start <- start + chunk_size
  }

  # restore the unsliced request in the handle
  curl::handle_setopt(handle = h, customrequest = fetch_request)

  paste0(pieces, collapse = "")
}

#' Size of the literal announced in a FETCH response
#' @param x The raw server response of a \code{FETCH} command.
#' @return The integer inside the \code{\{n\}} literal marker of the
#'   \code{* id FETCH (BODY[...] \{n\}} line, or \code{NA} if absent.
#' @noRd
literal_size <- function(x) {
  m <- regmatches(x, regexpr("\\* \\d+ FETCH \\([^{\r\n]*\\{\\d+\\}", x,
                             ignore.case = TRUE, useBytes = TRUE))
  if (length(m) == 0) {
    return(NA_integer_)
  }
  as.integer(sub(".*\\{(\\d+)\\}$", "\\1", m, useBytes = TRUE))
}
