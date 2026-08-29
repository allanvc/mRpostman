#' Criterion constructor referencing a filter stored on the server
#'
#' Builds a \code{FILTER <name>} search criterion (RFC 5466), which stands for
#' the search criteria saved on the server under that name through the
#' ManageSieve protocol. Requires the server \code{FILTERS} capability, which
#' is checked when the search is executed. Experimental: none of the
#' reference servers used to validate this package advertises \code{FILTERS},
#' so the criterion follows the RFC grammar but has not been exercised
#' against a live server.
#' @param name A \code{character} string with the name of the filter stored
#'   on the server.
#' @family custom search
#' @examples
#' \dontrun{
#' con$select_folder(name = "INBOX")
#' res <- con$search(request = AND(filter_stored("on-the-road"),
#'                                 string(expr = "boss@@example.com", where = "FROM")))
#' }
#'
#' @return A search criterion of class \code{imap_search}, to be combined
#'   into a search statement (see \code{Ops.imap_search}).
#' @export
#'
filter_stored <- function(name) {

  assertthat::assert_that(
    is.character(name), length(name) == 1,
    msg='"name" must be a single character string with the stored filter name.')

  as_imap_search(paste0("(FILTER ", name, ")"))

}
