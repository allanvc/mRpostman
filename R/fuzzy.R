#' Criterion modifier for fuzzy (approximate) matching in a custom search
#'
#' Prefixes a search criterion with \code{FUZZY} (RFC 6203), asking the server
#' to match the criterion approximately (typically through its full-text
#' search index) instead of by exact substring. Requires the server
#' \code{SEARCH=FUZZY} capability, which is checked when the search is
#' executed. Experimental: none of the reference servers used to validate
#' this package advertises \code{SEARCH=FUZZY}, so the modifier follows the
#' RFC grammar but has not been exercised against a live server.
#' @param criterion A search criterion string, usually built with
#'   \code{\link{string}} (e.g. \code{string(expr = "jump", where = "SUBJECT")}).
#' @family custom search
#' @examples
#' \dontrun{
#' con$select_folder(name = "INBOX")
#' res <- con$search(request = fuzzy(string(expr = "jump", where = "SUBJECT")))
#' }
#'
#' @return A search criterion of class \code{imap_search}, to be combined
#'   into a search statement (see \code{Ops.imap_search}).
#' @export
#'
fuzzy <- function(criterion) {

  assertthat::assert_that(
    is.character(criterion), length(criterion) == 1,
    msg='"criterion" must be a single search criterion string, e.g. built with string().')

  if (startsWith(criterion, "(")) {
    as_imap_search(sub("^\\(", "(FUZZY ", criterion))
  } else {
    as_imap_search(paste0("FUZZY ", criterion))
  }

}
