# A real MIME parser for the attachment helpers: splits a fetched message
# into its leaf parts by walking the multipart boundaries recursively,
# instead of scraping the raw text with regular expressions. Pure and
# offline-testable. Replaces the regex extraction that could truncate on
# "--" inside header values, misalign filenames and payloads, or mistake
# a header line for the payload (2.3.0).

#' Unfold and fetch one header value from a header block (INTERNAL HELPER)
#' @noRd
mime_header_value <- function(header_lines, name) {
  idx <- grep(paste0("^", name, ":"), header_lines, ignore.case = TRUE)
  if (length(idx) == 0) return(NA_character_)
  i <- idx[1]
  value <- sub(paste0("^", name, ":[ \t]*"), "", header_lines[i], ignore.case = TRUE)
  i <- i + 1
  while (i <= length(header_lines) && grepl("^[ \t]", header_lines[i])) {
    value <- paste0(value, " ", sub("^[ \t]+", "", header_lines[i]))
    i <- i + 1
  }
  value
}

#' Extract a MIME parameter, RFC 2231 continuations and encoding included (INTERNAL HELPER)
#' @noRd
mime_param <- function(value, param) {
  if (is.na(value)) return(NA_character_)
  # RFC 2231 extended form, possibly in numbered continuations:
  # param*=charset'lang'pct%20encoded  /  param*0*=...; param*1*=...
  ext <- regmatches(value, gregexpr(paste0("(?i)", param,
    "\\*(?:[0-9]+\\*?)?=(\"[^\"]*\"|[^;]+)"), value, perl = TRUE))[[1]]
  if (length(ext) > 0) {
    pieces <- sub(paste0("(?i)^", param, "\\*(?:[0-9]+\\*?)?="), "", ext, perl = TRUE)
    pieces <- gsub("^\"|\"$", "", trimws(pieces))
    joined <- paste(pieces, collapse = "")
    charset <- "UTF-8"
    m <- regmatches(joined, regexec("^([^']*)'[^']*'(.*)$", joined))[[1]]
    if (length(m) == 3) {
      if (nzchar(m[2])) charset <- m[2]
      joined <- m[3]
    }
    out <- tryCatch(utils::URLdecode(joined), warning = function(w) joined,
                    error = function(e) joined)
    conv <- suppressWarnings(iconv(out, from = charset, to = "UTF-8"))
    return(if (is.na(conv)) out else conv)
  }
  # plain form: param="quoted" or param=token
  m <- regmatches(value, regexec(paste0("(?i)", param,
    "[ \t]*=[ \t]*(\"([^\"]*)\"|[^;\r\n]+)"), value, perl = TRUE))[[1]]
  if (length(m) == 0) return(NA_character_)
  out <- if (nzchar(m[3]) || grepl('^"', m[2])) m[3] else trimws(m[2])
  decode_mime_header(out)
}

#' Split one message into its leaf MIME parts (INTERNAL HELPER)
#'
#' Walks the multipart tree by boundary, line-based (so CRLF and LF both
#' work), and returns one row per leaf part: \code{type},
#' \code{disposition} (\code{"attachment"}, \code{"inline"}, or NA),
#' \code{filename} (decoded; NA when the part declares none),
#' \code{encoding}, \code{text} (the undecoded body of the part), and
#' \code{payload} (a list column of raw vectors, transfer decoding
#' reversed).
#' @noRd
split_mime_parts <- function(msg) {
  empty <- data.frame(type = character(0), disposition = character(0),
                      filename = character(0), encoding = character(0),
                      text = character(0), stringsAsFactors = FALSE)
  empty$payload <- list()
  rows <- list()

  walk <- function(lines) {
    sep <- match(TRUE, lines == "")
    if (is.na(sep)) { headers <- lines; body <- character(0) }
    else { headers <- lines[seq_len(sep - 1)]; body <- lines[-seq_len(sep)] }
    ct <- mime_header_value(headers, "Content-Type")
    type <- if (is.na(ct)) "text/plain" else tolower(trimws(strsplit(ct, ";")[[1]][1]))
    if (grepl("^multipart/", type)) {
      b <- mime_param(ct, "boundary")
      if (is.na(b)) return(invisible(NULL))
      open <- paste0("--", b); close <- paste0("--", b, "--")
      marks <- which(trimws(body) == open | trimws(body) == close)
      if (length(marks) < 2) return(invisible(NULL))
      for (k in seq_len(length(marks) - 1)) {
        if (trimws(body[marks[k]]) == close) break
        seg <- body[(marks[k] + 1):(marks[k + 1] - 1)]
        walk(seg)
      }
      return(invisible(NULL))
    }
    cd <- mime_header_value(headers, "Content-Disposition")
    disposition <- if (is.na(cd)) NA_character_ else
      tolower(trimws(strsplit(cd, ";")[[1]][1]))
    if (!disposition %in% c("attachment", "inline")) disposition <- NA_character_
    filename <- mime_param(cd, "filename")
    if (is.na(filename)) filename <- mime_param(ct, "name")
    encoding <- mime_header_value(headers, "Content-Transfer-Encoding")
    encoding <- if (is.na(encoding)) NA_character_ else tolower(trimws(encoding))
    text <- paste(body, collapse = "\r\n")
    payload <- decode_part_raw(text, encoding)
    row <- data.frame(type = type, disposition = disposition,
                      filename = filename, encoding = encoding,
                      text = text, stringsAsFactors = FALSE)
    row$payload <- list(payload)
    rows[[length(rows) + 1]] <<- row
    invisible(NULL)
  }

  lines <- strsplit(msg, "\r\n|\n")[[1]]
  walk(lines)
  if (length(rows) == 0) return(empty)
  do.call(rbind, rows)
}

#' The attachment rows of split_mime_parts() (INTERNAL HELPER)
#'
#' Applies the content_disposition filter the attachment methods share:
#' \code{"both"} takes declared attachments and inline parts, plus
#' non-text leaves that carry a filename without any disposition.
#' @noRd
mime_attachment_parts <- function(parts, content_disposition = "both") {
  if (nrow(parts) == 0) return(parts)
  sel <- if (content_disposition == "both") {
    parts$disposition %in% c("attachment", "inline") |
      (is.na(parts$disposition) & !is.na(parts$filename) &
         !grepl("^multipart/", parts$type))
  } else {
    parts$disposition %in% content_disposition
  }
  parts[which(sel), , drop = FALSE]
}
