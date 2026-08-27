#' Tokenize an IMAP parenthesized list into nested R lists
#'
#' Parses the IMAP data structures used by \code{ENVELOPE} and
#' \code{BODYSTRUCTURE}: parenthesized lists, quoted strings (with
#' backslash escapes), literals (\code{\{n\}} followed by n bytes), atoms and
#' numbers (kept as character), and \code{NIL} (returned as \code{NA}).
#' @param x A \code{character} string starting at the opening parenthesis.
#' @return A nested \code{list}; scalar items are \code{character} strings or
#'   \code{NA}.
#' @noRd
imap_parse_list <- function(x) {
  b <- charToRaw(x)
  n <- length(b)
  pos <- 1L
  skip_ws <- function() {
    while (pos <= n && b[pos] %in% as.raw(c(0x20, 0x0d, 0x0a, 0x09))) pos <<- pos + 1L
  }
  parse_item <- function() {
    skip_ws()
    if (pos > n) return(NULL)
    ch <- b[pos]
    if (ch == as.raw(0x28)) {                       # "("
      pos <<- pos + 1L
      items <- list()
      repeat {
        skip_ws()
        if (pos > n) break
        if (b[pos] == as.raw(0x29)) { pos <<- pos + 1L; break }   # ")"
        items[[length(items) + 1L]] <- parse_item()
      }
      return(items)
    }
    if (ch == as.raw(0x22)) {                       # quoted string
      pos <<- pos + 1L
      out <- raw(0)
      while (pos <= n && b[pos] != as.raw(0x22)) {
        if (b[pos] == as.raw(0x5c) && pos < n) pos <<- pos + 1L   # backslash escape
        out <- c(out, b[pos]); pos <<- pos + 1L
      }
      pos <<- pos + 1L
      return(rawToChar(out))
    }
    if (ch == as.raw(0x7b)) {                       # literal {n}
      close <- pos
      while (close <= n && b[close] != as.raw(0x7d)) close <- close + 1L
      len <- as.integer(rawToChar(b[(pos + 1L):(close - 1L)]))
      pos <<- close + 1L
      if (pos <= n && b[pos] == as.raw(0x0d)) pos <<- pos + 1L
      if (pos <= n && b[pos] == as.raw(0x0a)) pos <<- pos + 1L
      out <- b[pos:min(n, pos + len - 1L)]
      pos <<- pos + len
      return(rawToChar(out))
    }
    start <- pos                                   # atom / number / NIL
    while (pos <= n && !(b[pos] %in% as.raw(c(0x20, 0x28, 0x29, 0x0d, 0x0a)))) pos <<- pos + 1L
    tok <- rawToChar(b[start:(pos - 1L)])
    if (toupper(tok) == "NIL") return(NA_character_)
    tok
  }
  parse_item()
}

# helpers ---------------------------------------------------------------------

imap_item <- function(lst, i) {
  if (length(lst) < i) return(NA_character_)
  v <- lst[[i]]
  if (is.null(v) || is.list(v)) return(NA_character_)
  v
}

imap_decode_words <- function(x) {
  if (is.na(x) || !nzchar(x) || !grepl("=\\?", x)) return(x)
  tryCatch(decode_mime_header(x), error = function(e) x)
}

imap_format_addresses <- function(lst) {
  if (is.null(lst) || !is.list(lst) || length(lst) == 0) return(NA_character_)
  addr <- vapply(lst, function(a) {
    if (!is.list(a)) return(NA_character_)
    name <- imap_decode_words(imap_item(a, 1))
    mailbox <- imap_item(a, 3); host <- imap_item(a, 4)
    email <- if (is.na(mailbox)) NA_character_ else if (is.na(host)) mailbox else paste0(mailbox, "@", host)
    if (is.na(email)) return(NA_character_)
    if (is.na(name) || !nzchar(name)) email else paste0(name, " <", email, ">")
  }, character(1))
  addr <- addr[!is.na(addr)]
  if (length(addr) == 0) NA_character_ else paste(addr, collapse = ", ")
}

imap_params <- function(lst) {
  if (is.null(lst) || !is.list(lst) || length(lst) < 2) return(character(0))
  keys <- unlist(lst[seq(1, length(lst) - 1, by = 2)])
  vals <- unlist(lst[seq(2, length(lst), by = 2)])
  stats::setNames(as.character(vals), tolower(as.character(keys)))
}

#' Parse an IMAP ENVELOPE into a one-row data frame
#'
#' Reads the \code{ENVELOPE (...)} structure of a \code{FETCH} response (RFC
#' 3501, section 7.4.2), as returned by \code{ImapCon$fetch_metadata(attribute
#' = "ENVELOPE")}, into analysis-ready columns. Address lists are formatted as
#' \code{Name <mailbox@host>} and joined with commas; RFC 2047 encoded words in
#' names and subjects are decoded.
#' @param x A \code{character} string with a \code{FETCH} response (or the
#'   part of it starting at \code{ENVELOPE}).
#' @return A one-row \code{data.frame} with columns \code{date},
#'   \code{subject}, \code{from}, \code{sender}, \code{reply_to}, \code{to},
#'   \code{cc}, \code{bcc}, \code{in_reply_to}, and \code{message_id}
#'   (\code{NA} where the message has no value).
#' @examples
#' x <- paste0('ENVELOPE ("Mon, 7 May 2001 08:41:00 -0700" "A resume" ',
#'             '(("Vince Kaminski" NIL "vince.kaminski" "enron.com")) NIL NIL ',
#'             '((NIL NIL "stephen.stock" "enron.com")) NIL NIL NIL "<id@x>")')
#' parse_envelope(x)
#' @export
parse_envelope <- function(x) {
  cols <- c("date", "subject", "from", "sender", "reply_to", "to", "cc", "bcc",
            "in_reply_to", "message_id")
  empty <- as.data.frame(as.list(stats::setNames(rep(NA_character_, length(cols)), cols)),
                         stringsAsFactors = FALSE)
  i <- regexpr("ENVELOPE\\s*\\(", x)
  if (i < 0) return(empty)
  lst <- imap_parse_list(substring(x, i + attr(i, "match.length") - 1))
  if (!is.list(lst) || length(lst) < 10) return(empty)
  data.frame(
    date        = imap_item(lst, 1),
    subject     = imap_decode_words(imap_item(lst, 2)),
    from        = imap_format_addresses(lst[[3]]),
    sender      = imap_format_addresses(lst[[4]]),
    reply_to    = imap_format_addresses(lst[[5]]),
    to          = imap_format_addresses(lst[[6]]),
    cc          = imap_format_addresses(lst[[7]]),
    bcc         = imap_format_addresses(lst[[8]]),
    in_reply_to = imap_item(lst, 9),
    message_id  = imap_item(lst, 10),
    stringsAsFactors = FALSE)
}

#' Parse an IMAP BODYSTRUCTURE into a data frame of MIME parts
#'
#' Reads the \code{BODYSTRUCTURE (...)} structure of a \code{FETCH} response
#' (RFC 3501, section 7.4.2), as returned by
#' \code{ImapCon$fetch_metadata(attribute = "BODYSTRUCTURE")}, into one row
#' per MIME part, numbered as in \code{FETCH BODY[<part>]}.
#' @param x A \code{character} string with a \code{FETCH} response (or the
#'   part of it starting at \code{BODYSTRUCTURE} or \code{BODY}).
#' @return A \code{data.frame} with columns \code{part} (the section number,
#'   e.g. \code{"1"}, \code{"2.1"}; multipart containers are listed with
#'   \code{part = NA}), \code{type}, \code{subtype}, \code{charset},
#'   \code{filename}, \code{encoding}, \code{size} (bytes, \code{NA} for
#'   containers), \code{disposition}, and \code{is_attachment}.
#' @examples
#' x <- paste0('BODYSTRUCTURE (("text" "plain" ("charset" "utf-8") NIL NIL ',
#'             '"quoted-printable" 120 3 NIL NIL NIL NIL)',
#'             '("application" "pdf" ("name" "report.pdf") NIL NIL "base64" ',
#'             '4096 NIL ("attachment" ("filename" "report.pdf")) NIL NIL) ',
#'             '"mixed" ("boundary" "xyz") NIL NIL NIL)')
#' parse_bodystructure(x)
#' @export
parse_bodystructure <- function(x) {
  cols <- c("part", "type", "subtype", "charset", "filename", "encoding",
            "size", "disposition", "is_attachment")
  empty <- data.frame(part = character(0), type = character(0), subtype = character(0),
                      charset = character(0), filename = character(0),
                      encoding = character(0), size = numeric(0),
                      disposition = character(0), is_attachment = logical(0),
                      stringsAsFactors = FALSE)
  i <- regexpr("BODY(STRUCTURE)?\\s*\\(", x)
  if (i < 0) return(empty)
  lst <- imap_parse_list(substring(x, i + attr(i, "match.length") - 1))
  if (!is.list(lst)) return(empty)
  rows <- list()
  walk <- function(node, prefix) {
    if (is.list(node[[1]])) {                         # multipart
      k <- 1
      while (k <= length(node) && is.list(node[[k]])) k <- k + 1
      subtype <- if (k <= length(node)) tolower(imap_item(node, k)) else NA_character_
      rows[[length(rows) + 1]] <<- data.frame(
        part = if (nzchar(prefix)) prefix else NA_character_, type = "multipart",
        subtype = subtype, charset = NA_character_, filename = NA_character_,
        encoding = NA_character_, size = NA_real_, disposition = NA_character_,
        is_attachment = FALSE, stringsAsFactors = FALSE)
      for (j in seq_len(k - 1)) {
        walk(node[[j]], if (nzchar(prefix)) paste0(prefix, ".", j) else as.character(j))
      }
      return(invisible())
    }
    type <- tolower(imap_item(node, 1)); subtype <- tolower(imap_item(node, 2))
    params <- imap_params(if (length(node) >= 3) node[[3]] else NULL)
    encoding <- tolower(imap_item(node, 6))
    size <- suppressWarnings(as.numeric(imap_item(node, 7)))
    # extension fields: after the basic ones (text: +lines; message/rfc822:
    # +envelope, body, lines), the first list met is the disposition
    start <- if (type == "text") 9 else if (type == "message" && subtype == "rfc822") 11 else 8
    disp <- NA_character_; disp_params <- character(0)
    if (length(node) >= start) {
      for (m in seq(start, length(node))) {
        v <- node[[m]]
        if (is.list(v) && length(v) >= 1 && !is.list(v[[1]])) {
          disp <- tolower(imap_item(v, 1))
          disp_params <- imap_params(if (length(v) >= 2) v[[2]] else NULL)
          break
        }
      }
    }
    filename <- if (!is.na(disp_params["filename"])) disp_params[["filename"]] else
      if (!is.na(params["name"])) params[["name"]] else NA_character_
    filename <- imap_decode_words(filename)
    rows[[length(rows) + 1]] <<- data.frame(
      part = if (nzchar(prefix)) prefix else "1", type = type, subtype = subtype,
      charset = if (!is.na(params["charset"])) tolower(params[["charset"]]) else NA_character_,
      filename = filename, encoding = encoding, size = size, disposition = disp,
      is_attachment = identical(disp, "attachment") || (!is.na(filename) && !identical(type, "text")),
      stringsAsFactors = FALSE)
    if (type == "message" && subtype == "rfc822" && length(node) >= 9 && is.list(node[[9]])) {
      walk(node[[9]], if (nzchar(prefix)) prefix else "1")
    }
    invisible()
  }
  walk(lst, "")
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[, cols]
}
