#' Criterion constructor functions for the SAVEDATE extension (RFC 8514)
#'
#' Build the \code{SAVEDBEFORE}, \code{SAVEDON}, and \code{SAVEDSINCE} search
#' keys, which compare the date a message was saved into the mailbox (the
#' \code{SAVEDATE} attribute) rather than its internal date or its
#' \code{Date:} header. They are to be combined with \code{\link{AND}} and
#' \code{\link{OR}} and passed to \code{ImapCon$search()}. The server must
#' advertise the \code{SAVEDATE} capability.
#' @param date_char A \code{character} string with a date in the IMAP format
#'   \code{DD-Mon-YYYY}, e.g. \code{"17-Apr-2019"}.
#' @param negate If \code{TRUE}, negates the search and seeks for "NOT SEARCH
#'   CRITERIA". Default is \code{FALSE}.
#' @return A \code{character} string with the search criterion.
#' @family custom search
#' @examples
#' \dontrun{
#' con$select_folder(name = "INBOX")
#' # messages saved into the folder during the last ingestion day
#' res <- con$search(request = saved_since(date_char = "27-Aug-2026"))
#' }
#' @export
saved_before <- function(date_char, negate = FALSE) {
  check_args(date_char = date_char, negate = negate)
  if (!isTRUE(negate)) as_imap_search(paste0('(SAVEDBEFORE ', date_char, ')'))
  else as_imap_search(paste0('(NOT (SAVEDBEFORE ', date_char, '))'))
}

#' @rdname saved_before
#' @export
saved_since <- function(date_char, negate = FALSE) {
  check_args(date_char = date_char, negate = negate)
  if (!isTRUE(negate)) as_imap_search(paste0('(SAVEDSINCE ', date_char, ')'))
  else as_imap_search(paste0('(NOT (SAVEDSINCE ', date_char, '))'))
}

#' @rdname saved_before
#' @export
saved_on <- function(date_char, negate = FALSE) {
  check_args(date_char = date_char, negate = negate)
  if (!isTRUE(negate)) as_imap_search(paste0('(SAVEDON ', date_char, ')'))
  else as_imap_search(paste0('(NOT (SAVEDON ', date_char, '))'))
}
