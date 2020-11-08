#' Decode RFC 2047 quoted-printable and base64 MIME encoded text
#' @param string A \code{character} vector containing a string to be decoded.
#' @return A decoded \code{character} vector if applicable.
#' @note The RFC 2047 (Moore, 1996) presents an encoded-word syntax to be used by e-mail
#'   clients to display body text and header information in character sets
#'   other than ASCII. According to the manual, non-ASCII content is encoded as
#'   an ASCII text string as follows: \code{=?<charset>?<encoding>?<encoded-text>?=}.
#'   The encoding can be of two types: "B" for "BASE64", or "Q" for quoted-
#'   printable content (Freed and Borentein, 1996). Besides the standard RFC 2047
#'   decoding, this function also enables users to decode content that does not
#'   strictly follow the \code{=?<charset>?<encoding>?<encoded-text>?=} RFC 2047
#'   syntax, i.e. cases where only the encoded text part is present, such as the
#'   quoted-printable pattern in the string \code{"Estat=EDstica"} (Estatística,
#'   which is the equivalent word, in Portuguese, for Statistics).
#' @references Moore, K. (1996), MIME (Multipurpose Internet Mail Extensions) Part
#'   Three: Message Header Extensions for Non-ASCII
#'   Text, RFC 2047, November 1996, https://tools.ietf.org/html/rfc2047.
#' @references Freed, N., Borenstein, N. (1996), Multipurpose Internet Mail Extensions
#'   (MIME) Part One: Format of Internet Message Bodies, RFC 2045, November 1996,
#'   https://tools.ietf.org/html/rfc2045.
#' @references Internal parts of this object, regarding the quoted printable type,
#'   were borrowed from https://github.com/hrbrmstr/hrbrmisc/blob/master/R/qp.r with
#'   slight modifications.
#' @noRd
decode_mime_text <- function(string) {

  # check if it is a character vector
  #check
  check_args(string = string)

  out <- c()
  for (i in seq_along(string)) { # "vectorized"


    content <- string[i]

    # first we need to test for b64 encoding
    # because the quoted-printable regex will also capture base64 encoded strings
    # but the contrary is not true
    if (grepl(pattern = "^([A-Za-z0-9+/]{4})*([A-Za-z0-9+/]{3}=|[A-Za-z0-9+/]{2}==)$", x = content)) {
      # sol: https://stackoverflow.com/questions/8571501/how-to-check-whether-a-string-is-base64-encoded-or-not
      decoded_string <- tryCatch({
        rawToChar(base64enc::base64decode(content))
      }, error = function(e) {
        content
      })

    } else if (grepl(pattern = "[\\x80-\\xff]", x = content)) { # assim reconhece direto sem precisar transformar!!
      decoded_string <- decode_quoted_printable_text(qp_encoded = content)

    } else {
      decoded_string <- content
    }

    out <- c(out, decoded_string)

  }

  return(out)

}
