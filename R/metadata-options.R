#' @title Message Metadata Options
#'
#' @description List Metadata fields used in messages.
#'
#' @return A \code{vector} containing message metadata fields.
#'
#' @note This function lists the message attributes that
#'     \href{#method-fetch_metadata}{\code{ImapCon$fetch_metadata()}} accepts.
#'     The last three (\code{PREVIEW}, \code{SAVEDATE}, \code{MODSEQ}) are
#'     capability-gated extensions, only sent when the server advertises them.
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
                  "BODYSTRUCTURE",
                  # extension attributes; fetch_metadata() checks the
                  # corresponding capability before requesting them:
                  "PREVIEW",   # RFC 8970
                  "SAVEDATE",  # RFC 8514
                  "MODSEQ")    # CONDSTORE, RFC 7162

  return(metadata_opts)
}
