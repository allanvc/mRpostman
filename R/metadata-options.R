#' @title Message Metadata Options
#'
#' @description List Metadata fields used in messages.
#'
#' @return A \code{vector} containing message metadata fields.
#'
#' @note This function lists message metadata used by
#'     IMAP servers, according to the RFC 2060 (Crispin, 1996).
#'
#' @references Crispin, M., "Internet Message Access Protocol - Version 4rev1",
#'     RFC 2060, \doi{10.17487/RFC2060}, December 1996,
#'     \url{https://www.rfc-editor.org/info/rfc2060}.
#'
#' @family options
#'
#' @examples
#' \dontrun{
#'
#' library(mRpostman)
#' metadata_options()
#'
#' }
#' @export
#'
metadata_options <- function() {

  metadata_opts <- c("INTERNALDATE", "UID", "ENVELOPE", "FLAGS", "RFC822.SIZE",
                  "BODYSTRUCTURE")

  return(metadata_opts)
}
