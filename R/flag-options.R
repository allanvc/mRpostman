#' @title Flag Options
#'
#' @description Lists flags used by most of IMAP servers.
#'
#' @return A \code{data.frame} containing flag names, its descriptions, and its
#'     antonym versions.
#'
#' @note RFC 2060 lists a set of systems flags, which are presentend and extended
#'     by this function. We also present a list of negative flags. When using
#'     \code{verbose = TRUE} in \link{configure_imap} function, flags appear
#'     with "\\", e.g. "\\Seen". It is important to note that some but not all
#'     functions that deal with flags also accepts, keywords (words listed by
#'     flags without starting with "\\"). This is the case of \link{add_flags},
#'     \link{remove_flags}, and \link{replace_flags}
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
#' flag_options()
#'
#' }
#' @export
#'
flag_options <- function() {

  flags <- c("SEEN", "ANSWERED", "FLAGGED", "DELETED", "DRAFT", "RECENT", "NEW")

  negate_flags <- c("UNSEEN", "UNANSWERED", "UNFLAGGED", "UNDELETED", "UNDRAFT", NA,
                        "OLD")

  description <- c("Message has been read", "Message has been answered",
                 "Message is 'flagged' for urgent/special attention",
                 "Message is 'deleted' for removal by later EXPUNGE",
                 "Message has not completed composition (marked as a draft)",
                "Message is 'recently' arrived in this mailbox (first session to have been notified)",
                NA)

  flags_df <- data.frame(flags, description, negate_flags)

  # but the user can pass other fields

  return(flags_df)
}

