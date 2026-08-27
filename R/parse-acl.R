#' Parse an ACL response (RFC 4314)
#'
#' Reads the untagged \code{* ACL <folder> <identifier> <rights> ...} line
#' into one row per identifier.
#' @param resp_char A \code{character} string with the server response.
#' @return A \code{data.frame} with columns \code{identifier} and
#'   \code{rights}, possibly empty.
#' @noRd
parse_acl <- function(resp_char) {
  lines <- strsplit(resp_char, "\r?\n")[[1]]
  acl_lines <- grep("^\\*\\s+ACL\\s", lines, value = TRUE)
  ids <- character(0); rights <- character(0)
  for (ln in unique(acl_lines)) {
    rest <- sub("^\\*\\s+ACL\\s+(?:\"[^\"]*\"|\\S+)\\s*", "", ln, perl = TRUE)
    toks <- strsplit(trimws(rest), "\\s+")[[1]]
    toks <- toks[nzchar(toks)]
    if (length(toks) < 2) next
    n <- length(toks) %/% 2
    ids <- c(ids, toks[seq(1, by = 2, length.out = n)])
    rights <- c(rights, toks[seq(2, by = 2, length.out = n)])
  }
  data.frame(identifier = ids, rights = rights, stringsAsFactors = FALSE)
}

#' Parse a LISTRIGHTS response (RFC 4314)
#'
#' Reads \code{* LISTRIGHTS <folder> <identifier> <required> <optional>...}.
#' @param resp_char A \code{character} string with the server response.
#' @return A \code{list} with elements \code{required} (a string) and
#'   \code{optional} (a character vector of right sets that may be granted),
#'   or \code{NULL} when the response carries no \code{LISTRIGHTS} line.
#' @noRd
parse_listrights <- function(resp_char) {
  lines <- strsplit(resp_char, "\r?\n")[[1]]
  ln <- grep("^\\*\\s+LISTRIGHTS\\s", lines, value = TRUE)
  if (length(ln) == 0) return(NULL)
  rest <- sub("^\\*\\s+LISTRIGHTS\\s+(?:\"[^\"]*\"|\\S+)\\s+(?:\"[^\"]*\"|\\S+)\\s*",
              "", ln[1], perl = TRUE)
  toks <- strsplit(trimws(rest), "\\s+")[[1]]
  toks <- toks[nzchar(toks)]
  if (length(toks) == 0) return(NULL)
  list(required = gsub('"', "", toks[1]), optional = toks[-1])
}

#' Parse a MYRIGHTS response (RFC 4314)
#' @param resp_char A \code{character} string with the server response.
#' @return A string with the rights, or \code{NA} when absent.
#' @noRd
parse_myrights <- function(resp_char) {
  m <- stringr::str_match(resp_char,
                          "\\*\\s+MYRIGHTS\\s+(?:\"[^\"]*\"|\\S+)\\s+(\\S+)")
  if (is.na(m[1, 2])) NA_character_ else m[1, 2]
}
