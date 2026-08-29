#' Criterion constructor function for the CONDSTORE extension (RFC 7162)
#'
#' Builds the \code{MODSEQ} search key, which matches the messages whose
#' modification sequence is equal to or greater than \code{value}, i.e. the
#' messages that were added or whose flags changed since the folder had that
#' \code{HIGHESTMODSEQ} (obtained with \code{ImapCon$status(items =
#' "HIGHESTMODSEQ")}). It is to be combined with \code{\link{AND}} and
#' \code{\link{OR}} and passed to \code{ImapCon$search()}. The server must
#' advertise the \code{CONDSTORE} capability.
#' @param value A single non-negative number, the modification sequence to
#'   compare with.
#' @param negate If \code{TRUE}, negates the search and seeks for "NOT SEARCH
#'   CRITERIA". Default is \code{FALSE}.
#' @return A \code{character} string with the search criterion.
#' @family custom search
#' @examples
#' \dontrun{
#' con$select_folder(folder = "INBOX")
#' last <- con$status(items = "HIGHESTMODSEQ")[["HIGHESTMODSEQ"]]
#' # ... later, in another run:
#' changed <- con$search(request = modseq(last + 1))
#' }
#' @export
modseq <- function(value, negate = FALSE) {
  assertthat::assert_that(is.numeric(value), length(value) == 1, value >= 0,
                          msg='"value" must be a single non-negative number.')
  assertthat::assert_that(is.logical(negate), msg='"negate" must be a logical.')
  v <- format(value, scientific = FALSE)
  if (!isTRUE(negate)) as_imap_search(paste0('(MODSEQ ', v, ')'))
  else as_imap_search(paste0('(NOT (MODSEQ ', v, '))'))
}
