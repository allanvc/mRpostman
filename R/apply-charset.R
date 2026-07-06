#' Convert a byte string from a declared charset to UTF-8
#'
#' Helper used by the MIME/RFC 2047 decoders. It takes a character vector whose
#' bytes are already in the source charset (e.g. the output of a
#' quoted-printable or base64 decode step) and the charset label declared in the
#' encoded-word (\code{=?<charset>?<enc>?...?=}) or MIME \code{Content-Type}, and
#' re-encodes it to UTF-8 via \code{iconv}. This replaces the previous heuristic
#' charset guessing and enables any charset that \code{iconv} supports
#' (e.g. Windows-1251, ISO-8859-2, Big5, Shift_JIS, EUC-KR).
#'
#' It is intentionally defensive: an unknown/unsupported charset label, or bytes
#' that cannot be translated, fall back to the original input instead of erroring
#' or returning \code{NA}.
#' @param x A \code{character} vector holding bytes in \code{charset}.
#' @param charset A \code{character} string with the source charset label. If
#'   \code{NULL}, \code{NA} or empty, \code{x} is returned unchanged.
#' @return A \code{character} vector re-encoded to UTF-8 where possible.
#' @noRd
apply_charset <- function(x, charset = NULL) {

  if (is.null(charset) || length(charset) == 0 || is.na(charset[1]) ||
      !nzchar(trimws(charset[1]))) {
    return(x)
  }

  cs <- toupper(trimws(charset[1]))

  # US-ASCII is a strict subset of UTF-8, nothing to convert.
  if (cs %in% c("US-ASCII", "ASCII", "ANSI_X3.4-1968")) {
    return(x)
  }

  # Try the declared charset. iconv() returns NA per untranslatable element and,
  # for an unsupported charset label, all-NA (with a warning) or errors depending
  # on the platform -- handle every case by falling back to the original bytes.
  out <- tryCatch(
    suppressWarnings(iconv(x, from = charset[1], to = "UTF-8")),
    error = function(e) NULL
  )

  if (is.null(out) || all(is.na(out))) {
    return(x)
  }

  # Keep the original for any element iconv could not translate.
  out[is.na(out)] <- x[is.na(out)]

  out

}
