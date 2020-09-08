#' @title Section or Header Fields Options
#'
#' @description This function presents message sections and header fields
#'     that can be used for searching and fetching message contents. The header
#'     fields, particularly, are only a suggestion and indicate the most common
#'     terms. Some messages may not contain one or more of the listed fields.
#'     It is also possible that one or more fields not listed here may be used
#'     in some messages.
#'
#' @return A \code{list} containing common header fields.
#'
#' @note A message body is the (full) content of a message. It is simply lines
#'     of US-ASCII characters (Freed & Borenstein, 1996; Resnick, 2001). Therefore,
#'     if you want to do a search of a string in the whole message, including
#'     \code{HEADER} and \code{TEXT}, you can specify
#'     \code{section_or_field = "BODY"}.
#'
#' @note While header is a part of the message body, indicating sender, date and
#'     other information, header fields are specific
#'     parts of the header of a message. "Header fields are lines
#'     composed of a field name, followed by a colon (":"), which is followed by
#'     a field body, and terminated by CRLF" (Resnick, 2001).
#'
#' @note A message \code{TEXT} is the message itself, indicating the content
#'     written or produced by the sender.
#'
#' @references Freed, N. and N. Borenstein, "Multipurpose Internet Mail
#'     Extensions (MIME) Part Two: Media Types", RFC 2046,
#'     \doi{10.17487/RFC2046}, November 1996,
#'     \url{https://www.rfc-editor.org/info/rfc2046}.
#'
#' @references Resnick, P., Ed., "Internet Message Format", RFC 2822,
#'     \doi{10.17487/RFC2822}, April 2001,
#'     \url{https://www.rfc-editor.org/info/rfc2822}.
#'
#' @references Crocker, D., "STANDARD FOR THE FORMAT OF ARPA INTERNET TEXT
#'     MESSAGES", STD 11, RFC 822, \doi{10.17487/RFC0822},
#'     August 1982, \url{https://www.rfc-editor.org/info/rfc822}.
#'
#' @family options
#'
#' @examples
#' \dontrun{
#'
#' library(mRpostman)
#' section_or_field_options()
#'
#' }
#' @export
#'
section_or_field_options <- function() {

  section_opts <- c("BODY", "HEADER", "TEXT")


  headerFields_opts <- c("From", "To", "Cc", "Bcc", "Subject", "Date", "Delivered-To",
                   "In-Reply-To", "Received", "Return-Path", "Received-SPF",
                   "Authentication-Results", "Message-ID", "MIME-Version",
                   "Content-Type", "X-Mailer", "X-MimeOLE", "X-Terra-Karma",
                   "X-Terra-Hash", "X-Priority", "X-MSMail-Priority",
                   "X-Originating-IP", "Disposition-Notification-To",
                   "X-CLX-Rate-Response", "References")

  section_or_field <- list("sections" = section_opts)

  section_or_field[["HeaderFields"]] <- headerFields_opts
  # but the user can pass other fields

  return(section_or_field)
}
