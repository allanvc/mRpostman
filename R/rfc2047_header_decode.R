#' Decode RFC 2047 quoted-printable and base64 headers or strings
#' @param header A \code{character} vector containing a string to be decoded.
#' @return A decoded \code{character} vector if applicable.
#' @export
#' @examples
#' # Simple quoted-printable string - Portuguese example
#' qp_encoded <- "Minist=E9rio_da_Educa=E7=E3o"
#' decoded_string <- rfc2047_header_decode(header = qp_encoded)
#'
#' # Simple quoted-printable string - French example
#' qp_encoded <- "Minist=E9rio_da_Educa=E7=E3o"
#' decoded_string <- rfc2047_header_decode(header = qp_encoded)
#'
#' # RFC 2047 quoted-printable header - Portuguese example
#' qp_encoded <- "=?iso-8859-1?Q?DIDEC_Capacita=E7=E3o?="
#' decoded_string <- rfc2047_header_decode(header = qp_encoded)
#'
#' # RFC 2047 quoted-printable - German example
#' qp_encoded <- "=?UTF-8?Q?stern=2Ede_-_t=C3=A4glich?="
#' decoded_string <- rfc2047_header_decode(header = qp_encoded)
#'
#' # RFC 2047 base64 - Portuguese example
#' b64_encoded <- "=?utf-8?B?Sk9BTkEgRlVTQ08gTE9CTyBubyBUZWFtcw==?="
#' decoded_string <- rfc2047_header_decode(header = b64_encoded)
#'
rfc2047_header_decode <- function(header) {

  # check if it is a character vector
  #check
  # check_args(header = header)

  out <- c()
  for (i in seq_along(header)) { # "vectorized"

    # i = 1

    x_split <- unlist(strsplit(header[i], "\\?"))

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
        decoded_string <- decode_quoted_printable(qp_encoded = content)
      } else if (encoding == "B" | encoding == "b") {
        decoded_string <- rawToChar(base64enc::base64decode(content))
      } else {
        decoded_string <- content
      }

    } else {

      content <- header[i]

      # first we need to test for b64 encoding
      # because the quoted-printable regex will also capture base64 encoded strings
      # but the contrary is not true
      if (grepl(pattern = "^([A-Za-z0-9+/]{4})*([A-Za-z0-9+/]{3}=|[A-Za-z0-9+/]{2}==)?$", x = content)) {
        # sol: https://stackoverflow.com/questions/8571501/how-to-check-whether-a-string-is-base64-encoded-or-not
        decoded_string <- tryCatch({
          rawToChar(base64enc::base64decode(content))
        }, error = function(e) {
          content
        })

      } else if (grepl(pattern = "[\\x80-\\xff]", x = content)) { # assim reconhece direto sem precisar transformar!!
        decoded_string <- decode_quoted_printable(qp_encoded = content)

      } else {
        decoded_string <- content
      }

    }

    out <- c(out, decoded_string)

  }

  return(out)

}
