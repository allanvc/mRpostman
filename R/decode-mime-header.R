#' Decode RFC 2047 quoted-printable and base64 MIME headers and strings
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
#' @export
#' @examples
#' \dontrun{
#' # The examples below runs smoothly on any computer. The 'dontrun' flag is just to skip CRAN checks.
#'
#' # Simple quoted-printable string - Portuguese example
#' qp_encoded <- "Minist=E9rio_da_Educa=E7=E3o"
#' decode_mime_header(string = qp_encoded)
#'
#' # Simple quoted-printable string - French example
#' qp_encoded <- "sur la route =C3=A0 suivre les voil=C3=A0 bient=C3=B4t qui te d=C3=A9gradent"
#' decode_mime_header(string = qp_encoded)
#'
#' # Simple quoted-printable string - Norwegian example
#' qp_encoded <- "p=C3=A5 veien for =C3=A5 f=C3=B8lge, snart vil de forringe deg"
#' decode_mime_header(string = qp_encoded)
#'
#' # Simple quoted-printable string - Turkish example
#' qp_encoded <- "yak=C4=B1nda seni k=C3=BC=C3=A7=C3=BCk d=C3=BC=C5=9F=C3=BCrecekler"
#' decode_mime_header(string = qp_encoded)
#'
#' # RFC 2047 quoted-printable header - Portuguese example
#' qp_encoded <- "=?iso-8859-1?Q?DIDEC_Capacita=E7=E3o?="
#' decode_mime_header(string = qp_encoded)
#'
#' # RFC 2047 quoted-printable - German example
#' qp_encoded <- "=?UTF-8?Q?stern=2Ede_-_t=C3=A4glich?="
#' decode_mime_header(string = qp_encoded)
#'
#' # RFC 2047 base64 - Portuguese example
#' b64_encoded <- "=?utf-8?B?Sk9BTkEgRlVTQ08gTE9CTyBubyBUZWFtcw==?="
#' decode_mime_header(string = b64_encoded)
#' }
#'
decode_mime_header <- function(string) {

  # check if it is a character vector
  #check
  # string = qp_encoded
  check_args(string = string)

  out <- c()
  for (i in seq_along(string)) { # "vectorized"

    # i = 1

    x_split <- unlist(strsplit(string[i], "\\?"))

    # eliminate repetitions (it happens when a large name occupy more than one line)
    x_split <- unique(x_split)

    x_split <- x_split[x_split != '=\r\n =']

    x_split <- x_split[x_split != '= =']

    x_split <- x_split[x_split != '=\r\n=']

    x_split <- x_split[x_split != '\r\n']

    x_split <- x_split[x_split != '==']

    # x_split <- gsub("^[=]", "", x_split)

    x_split <- x_split[x_split != "="]

    x_split <- x_split[x_split != ""]

    # when it is a large string occupying more than one line, like:
    # "=?Windows-1252?Q?Termo_de_responsabilidade_-_remunera=E7=E3o_extra_SIAPE.?=\r\n=?Windows-1252?Q?pdf?="
    if (length(x_split) > 3) {
      x_split[3] <- paste0(x_split[3:length(x_split)], collapse = "")
      x_split <- x_split[1:3]
    }

    if (length(x_split) == 3) {

      charset <- x_split[1]

      encoding <- x_split[2]

      content <- x_split[3]

      if (encoding == "Q" | encoding == "q") {
        decoded_string <- decode_quoted_printable_header(qp_encoded = content)
      } else if (encoding == "B" | encoding == "b") {
        decoded_string <- rawToChar(base64enc::base64decode(content))
      } else {
        decoded_string <- content
      }

    } else {

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

      # } else if (grepl(pattern = "[\\x80-\\xff]", x = content)) { # assim reconhece direto sem precisar transformar!!
      } else if (grepl(pattern = "[\\x80-\\xD1\\x8F]", x = content)) { # assim reconhece direto sem precisar transformar!!

        decoded_string <- decode_quoted_printable_header(qp_encoded = content)

      } else {
        decoded_string <- content
      }

    }

    out <- c(out, decoded_string)

  }

  return(out)

}
