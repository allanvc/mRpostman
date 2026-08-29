#' Criterion constructor function for a verbatim IMAP search fragment
#'
#' Wraps any raw IMAP \code{SEARCH} fragment as a search criterion, so that
#' protocol keys without a dedicated field in
#' \href{#method-query}{\code{ImapCon$query()}} - vendor extensions such as
#' Gmail's \code{X-GM-RAW}, sequence sets such as \code{UID 100:200}, or
#' \code{FUZZY} matches (RFC 6203) - can be combined with the rest of the
#' search language or with the other criterion constructors. The fragment is
#' passed to the server exactly as provided (parenthesized if it is not
#' already), and its keys are not validated locally: an unknown key results
#' in a server-side \code{BAD} response.
#' @param request A character string with the raw search fragment.
#' @family custom search
#' @examples
#' \dontrun{
#' # a vendor extension combined with a regular field
#' res <- con$query(verbatim('X-GM-RAW "has:attachment smaller:25M"') &
#'                    flag != "SEEN")
#' # a UID range in a classic custom search
#' res <- con$search(request = AND(verbatim("UID 100:200"),
#'                                 string(expr = "@@gmail.com", where = "FROM")),
#'                   use_uid = TRUE)
#' }
#'
#' @export
#'
verbatim <- function(request) {

  assertthat::assert_that(
    is.character(request), length(request) == 1, !is.na(request),
    nzchar(trimws(request)),
    msg = '"request" must be a single non-empty string.')

  out <- trimws(request)

  if (!is_single_paren_group(out)) {
    out <- paste0("(", out, ")")
  }

  return(as_imap_search(out))

}

# TRUE when the string is one balanced "( ... )" group, i.e. already a single
# parenthesized key that can stand alone inside AND/OR/NOT
#' @noRd
is_single_paren_group <- function(x) {
  chars <- strsplit(x, "", fixed = TRUE)[[1]]
  if (length(chars) < 2 || chars[1] != "(" || chars[length(chars)] != ")") {
    return(FALSE)
  }
  depth <- 0L
  in_quotes <- FALSE
  for (i in seq_along(chars)) {
    ch <- chars[i]
    if (in_quotes) {
      if (ch == '"' && chars[i - 1] != "\\") in_quotes <- FALSE
      next
    }
    if (ch == '"') { in_quotes <- TRUE; next }
    if (ch == "(") depth <- depth + 1L
    if (ch == ")") {
      depth <- depth - 1L
      if (depth == 0L && i < length(chars)) return(FALSE)
    }
  }
  return(depth == 0L && !in_quotes)
}
