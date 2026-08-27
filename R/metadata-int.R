# METADATA extension (RFC 5464): GETMETADATA / SETMETADATA on a mailbox or on
# the server (name = NULL -> the empty mailbox name "").

metadata_mailbox <- function(name) {
  if (is.null(name) || identical(name, "")) '""' else adjust_folder_name(name)
}

imap_quote <- function(x) {
  paste0('"', gsub('(["\\\\])', "\\\\\\1", x), '"')
}

#' @noRd
get_metadata_int <- function(self, name, entries, depth, max_size, retries) {
  assertthat::assert_that(is.character(entries), length(entries) >= 1,
                          all(startsWith(entries, "/")),
                          msg='"entries" must be a character vector of entry names starting with "/" (e.g. "/private/comment").')
  if (!is.null(depth)) {
    assertthat::assert_that(depth %in% c("0", "1", "infinity", 0, 1),
                            msg='"depth" must be NULL, "0", "1", or "infinity".')
  }
  if (!is.null(max_size)) {
    assertthat::assert_that(is.numeric(max_size), length(max_size) == 1, max_size >= 0,
                            msg='"max_size" must be NULL or a single non-negative number.')
  }
  check_args(retries = retries)
  assert_capability(self, "METADATA", command = "get_metadata", rfc = "RFC 5464",
                    prefix = TRUE, retries = retries)
  opts <- c(if (!is.null(depth)) paste("DEPTH", as.character(depth)),
            if (!is.null(max_size)) paste("MAXSIZE", format(max_size, scientific = FALSE)))
  opts_str <- if (length(opts)) paste0("(", paste(opts, collapse = " "), ") ") else ""
  # RFC 5464, section 4.2: the options precede the mailbox name
  customrequest <- paste0("GETMETADATA ", opts_str, metadata_mailbox(name), " ",
                          "(", paste(imap_quote(entries), collapse = " "), ")")
  parse_metadata(execute_simple_command(self, customrequest, retries))
}

#' @noRd
set_metadata_int <- function(self, name, entries, retries) {
  # c("/private/comment" = NA) is a logical vector in R: coerce, keeping names
  if (is.logical(entries) && all(is.na(entries))) {
    nm <- names(entries)
    entries <- rep(NA_character_, length(entries))
    names(entries) <- nm
  }
  assertthat::assert_that(is.character(entries), length(entries) >= 1,
                          !is.null(names(entries)), all(startsWith(names(entries), "/")),
                          msg='"entries" must be a named character vector: names are the entries (e.g. "/private/comment"), values the new values (NA removes the entry).')
  assertthat::assert_that(!any(grepl("[\r\n]", entries)),
                          msg='metadata values cannot contain line breaks (IMAP literals are not supported by the transport).')
  check_args(retries = retries)
  assert_capability(self, "METADATA", command = "set_metadata", rfc = "RFC 5464",
                    prefix = TRUE, retries = retries)
  pairs <- vapply(seq_along(entries), function(i) {
    paste(imap_quote(names(entries)[i]), if (is.na(entries[i])) "NIL" else imap_quote(entries[i]))
  }, character(1))
  customrequest <- paste0("SETMETADATA ", metadata_mailbox(name), " (", paste(pairs, collapse = " "), ")")
  execute_simple_command(self, customrequest, retries)
  invisible(TRUE)
}

#' Parse METADATA responses into a data frame of entries and values
#' @param resp_char A \code{character} string with the server response.
#' @return A \code{data.frame} with columns \code{mailbox}, \code{entry}, and
#'   \code{value} (\code{NA} for \code{NIL}).
#' @noRd
parse_metadata <- function(resp_char) {
  out <- data.frame(mailbox = character(0), entry = character(0), value = character(0),
                    stringsAsFactors = FALSE)
  starts <- gregexpr("\\*\\s+METADATA\\s+", resp_char)[[1]]
  if (starts[1] < 0) return(out)
  for (st in starts) {
    rest <- substring(resp_char, st)
    # mailbox name (quoted or not), then the parenthesized entry list
    mm <- stringr::str_match(rest, "^\\*\\s+METADATA\\s+(?:\"((?:[^\"\\\\]|\\\\.)*)\"|(\\S+))\\s*")
    mailbox <- if (!is.na(mm[1, 2])) mm[1, 2] else mm[1, 3]
    lst <- imap_parse_list(substring(rest, nchar(mm[1, 1]) + 1))
    if (!is.list(lst) || length(lst) < 2) next
    keys <- unlist(lapply(lst[seq(1, length(lst) - 1, by = 2)], function(x) if (is.character(x)) x else NA_character_))
    vals <- unlist(lapply(lst[seq(2, length(lst), by = 2)], function(x) if (is.character(x)) x else NA_character_))
    out <- rbind(out, data.frame(mailbox = imap_utf7_decode(mailbox), entry = keys, value = vals,
                                 stringsAsFactors = FALSE))
  }
  out <- out[!duplicated(out[, c("mailbox", "entry")]), , drop = FALSE]
  rownames(out) <- NULL
  out
}
