#' Encode and decode mailbox names in IMAP modified UTF-7
#'
#' IMAP mailbox names are transmitted in the modified UTF-7 encoding of RFC
#' 3501 (section 5.1.3): non-ASCII runs are written as \code{&<base64>-},
#' where the base64 alphabet uses \code{,} instead of \code{/} and carries no
#' padding, and a literal \code{&} is written \code{&-}. \code{mRpostman}
#' applies the encoding to every folder name it sends and decodes the names
#' it receives, so folders can be referred to by their real (UTF-8) names;
#' these helpers are exported for users who handle raw names.
#' @param x A \code{character} vector of mailbox names.
#' @return A \code{character} vector of the same length.
#' @examples
#' imap_utf7_encode("École")   # "&AMk-cole"
#' imap_utf7_decode("&AMk-cole")    # "École"
#' imap_utf7_encode("Q&A")          # "Q&-A"
#' @export
imap_utf7_encode <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    s <- enc2utf8(s)
    chars <- strsplit(s, "", fixed = TRUE)[[1]]
    if (length(chars) == 0) return("")
    cp <- utf8ToInt(s)
    direct <- cp >= 0x20 & cp <= 0x7E & cp != 0x26   # printable ASCII except "&"
    out <- character(0)
    i <- 1
    while (i <= length(chars)) {
      if (chars[i] == "&") {
        out <- c(out, "&-"); i <- i + 1
      } else if (direct[i]) {
        j <- i
        while (j <= length(chars) && direct[j]) j <- j + 1
        out <- c(out, paste(chars[i:(j - 1)], collapse = "")); i <- j
      } else {
        j <- i
        while (j <= length(chars) && !direct[j] && chars[j] != "&") j <- j + 1
        run <- paste(chars[i:(j - 1)], collapse = "")
        b <- stringi::stri_encode(run, from = "UTF-8", to = "UTF-16BE", to_raw = TRUE)[[1]]
        b64 <- base64enc::base64encode(b)
        b64 <- gsub("=", "", gsub("/", ",", b64, fixed = TRUE), fixed = TRUE)
        out <- c(out, "&", b64, "-"); i <- j
      }
    }
    paste(out, collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

#' @rdname imap_utf7_encode
#' @export
imap_utf7_decode <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    if (!grepl("&", s, fixed = TRUE)) return(enc2utf8(s))
    m <- gregexpr("&[A-Za-z0-9+,]*-", s)
    pieces <- regmatches(s, m)[[1]]
    if (length(pieces) == 0) return(enc2utf8(s))
    decoded <- vapply(pieces, function(p) {
      if (p == "&-") return("&")
      b64 <- substr(p, 2, nchar(p) - 1)
      b64 <- gsub(",", "/", b64, fixed = TRUE)
      pad <- (4 - nchar(b64) %% 4) %% 4
      raw <- tryCatch(base64enc::base64decode(paste0(b64, strrep("=", pad))),
                      error = function(e) NULL)
      if (is.null(raw)) return(p)
      stringi::stri_encode(raw, from = "UTF-16BE", to = "UTF-8")
    }, character(1))
    regmatches(s, m)[[1]] <- decoded
    enc2utf8(s)
  }, character(1), USE.NAMES = FALSE)
}
