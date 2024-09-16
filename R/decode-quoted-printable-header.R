#' Decode quoted-printable header
#' @param qp_encoded A \code{character} vector containing a quoted-printable string
#'   to be decoded.
#' @noRd
decode_quoted_printable_header <- function (qp_encoded) {
  # adapted from @hbrmstr original function:
  # https://stackoverflow.com/questions/40976213/decoding-quoted-printable-string-in-r

  qp_before <- c("=00", "=01", "=02", "=03", "=04", "=05", "=06", "=07", "=08", "=09", "=0A",
                 "=0B", "=0C", "=0D", "=0E", "=0F", "=10", "=11", "=12", "=13", "=14", "=15",
                 "=16", "=17", "=18", "=19", "=1A", "=1B", "=1C", "=1D", "=1E", "=1F", "=20",
                 "=21", "=22", "=23", "=24", "=25", "=26", "=27", "=28", "=29", "=2A", "=2B",
                 "=2C", "=2D", "=2E", "=2F", "=30", "=31", "=32", "=33", "=34", "=35", "=36",
                 "=37", "=38", "=39", "=3A", "=3B", "=3C", "=3D", "=3E", "=3F", "=40",
                 "=41",
                 "=42", "=43", "=44", "=45", "=46", "=47", "=48", "=49", "=4A", "=4B", "=4C",
                 "=4D", "=4E", "=4F", "=50", "=51", "=52", "=53", "=54", "=55", "=56", "=57",
                 "=58", "=59", "=5A",
                 "=5B", "=5C", "=5D", "=5E", "=5F", "=60", "=61", "=62",
                 "=63", "=64", "=65", "=66", "=67", "=68", "=69", "=6A", "=6B", "=6C", "=6D",
                 "=6E", "=6F", "=70", "=71", "=72", "=73", "=74", "=75", "=76", "=77", "=78",
                 "=79", "=7A", "=7B", "=7C", "=7D", "=7E",
                 "=7F", "=80", "=81", "=82", "=83",
                 "=84", "=85", "=86", "=87", "=88", "=89", "=8A", "=8B", "=8C", "=8D", "=8E",
                 "=8F", "=90", "=91", "=92", "=93", "=94", "=95", "=96", "=97", "=98", "=99",
                 "=9A", "=9B", "=9C", "=9D", "=9E", "=9F", "=A0", "=A1", "=A2", "=A3", "=A4",
                 "=A5", "=A6", "=A7", "=A8", "=A9", "=AA", "=AB", "=AC", "=AD", "=AE", "=AF",
                 "=B0", "=B1", "=B2", "=B3", "=B4", "=B5", "=B6", "=B7", "=B8", "=B9", "=BA",
                 "=BB", "=BC", "=BD", "=BE", "=BF", "=C0", "=C1", "=C2", "=C3", "=C4", "=C5",
                 "=C6", "=C7", "=C8", "=C9", "=CA", "=CB", "=CC", "=CD", "=CE", "=CF", "=D0",
                 "=D1", "=D2", "=D3", "=D4", "=D5", "=D6", "=D7", "=D8", "=D9", "=DA", "=DB",
                 "=DC", "=DD", "=DE", "=DF", "=E0", "=E1", "=E2", "=E3", "=E4", "=E5", "=E6",
                 "=E7", "=E8", "=E9", "=EA", "=EB", "=EC", "=ED", "=EE", "=EF", "=F0", "=F1",
                 "=F2", "=F3", "=F4", "=F5", "=F6", "=F7", "=F8", "=F9", "=FA", "=FB", "=FC",
                 "=FD", "=FE", "=FF", "=\r\n")

  qp_after <- c("", "\001", "\002", "\003", "\004", "\005", "\006", "\a", "\b", "\t", "\n",
                "\v", "\f", "\r", "\016", "\017", "\020", "\021", "\022", "\023", "\024", "\025",
                "\026", "\027", "\030", "\031", "\032", "\033", "\034", "\035", "\036", "\037", " ",
                "!", "\"", "#", "$", "%", "&", "'", "(", ")", "*", "+",
                ",", "-", ".", "/", "0", "1", "2", "3", "4", "5", "6",
                "7", "8", "9", ":", ";", "<", "=", ">", "?", "@",
                "A",
                "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L",
                "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W",
                "X", "Y", "Z",
                "[", "\\", "]", "^", "_", "`", "a", "b",
                "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
                "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x",
                "y", "z", "{", "|", "}", "~",
                "\177", "\x80", "\x81", "\x82",
                "\x83", "\x84", "\x85", "\x86", "\x87", "\x88", "\x89", "\x8a", "\x8b",
                "\x8c", "\x8d", "\x8e", "\x8f", "\x90", "\x91", "\x92", "\x93", "\x94",
                "\x95", "\x96", "\x97", "\x98", "\x99", "\x9a", "\x9b", "\x9c", "\x9d",
                "\x9e", "\x9f", "\xa0", "\xa1", "\xa2", "\xa3", "\xa4", "\xa5", "\xa6",
                "\xa7", "\xa8", "\xa9", "\xaa", "\xab", "\xac", "\xad", "\xae", "\xaf",
                "\xb0", "\xb1", "\xb2", "\xb3", "\xb4", "\xb5", "\xb6", "\xb7", "\xb8",
                "\xb9", "\xba", "\xbb", "\xbc", "\xbd", "\xbe", "\xbf", "\xc0", "\xc1",
                "\xc2", "\xc3", "\xc4", "\xc5", "\xc6", "\xc7", "\xc8", "\xc9", "\xca",
                "\xcb", "\xcc", "\xcd", "\xce", "\xcf", "\xd0", "\xd1", "\xd2", "\xd3",
                "\xd4", "\xd5", "\xd6", "\xd7", "\xd8", "\xd9", "\xda", "\xdb", "\xdc",
                "\xdd", "\xde", "\xdf", "\xe0", "\xe1", "\xe2", "\xe3", "\xe4", "\xe5",
                "\xe6", "\xe7", "\xe8", "\xe9", "\xea", "\xeb", "\xec", "\xed", "\xee",
                "\xef", "\xf0", "\xf1", "\xf2", "\xf3", "\xf4", "\xf5", "\xf6", "\xf7",
                "\xf8", "\xf9", "\xfa", "\xfb", "\xfc", "\xfd", "\xfe", "\xff", "")


  utf8_codes <- c("=C3=A9", "=C3=A7", "=C3=A3", "=C3=A1", "=C3=AA", "=C3=B3", "=C3=B4", "=C3=BC", "=C3=AD", "=C3=BA")

  latin1_codes <- c("=E9", "=E7", "=E3", "=E1", "=EA", "=F3", "=F4", "=FC", "=ED", "=FA")

  # vector for ISO-8859-10 (Nordic/Latin-6)
  iso_8859_10_codes <- c("=F0", "=FE", "=E6", "=F8", "=E5")
  # vector for Windows-1252
  windows_1252_codes <- c("=80", "=82", "=83", "=84", "=85", "=86", "=87", "=88", "=89")

  # iso_8859_5_codes <- c("=D0=90", "=D0=91", "=D0=92", "=D0=93", "=D0=94")  # ISO-8859-5 for russian
  # koi8_r_codes <- c("=E1", "=E2", "=E3", "=E4", "=E5")  # KOI8-R for russian
  iso_8859_9_codes <- c("=E7", "=FC", "=F6", "=FD", "=FE")  # ISO-8859-9 para turco
  windows_1254_codes <- c("=E7", "=FC", "=F6", "=FD", "=FE")  # Windows-1254 para turco


  # check latin1 code in string
  latin1_present <- any(sapply(latin1_codes, grepl, qp_encoded, fixed = TRUE))

  # check UTF-88 code in string
  utf8_present <- any(sapply(utf8_codes, grepl, qp_encoded, fixed = TRUE))

  # check ISO-8859-10 (Nordic) code in string
  iso_8859_10_present <- any(sapply(iso_8859_10_codes, grepl, qp_encoded, fixed = TRUE))

  # check Windows-1252 code in string
  windows_1252_present <- any(sapply(windows_1252_codes, grepl, qp_encoded, fixed = TRUE))

  # ISO-8859-5 (Cyrillic)
  # iso_8859_5_present <- any(sapply(iso_8859_5_codes, grepl, qp_encoded, fixed = TRUE))

  # KOI8-R
  # koi8_r_present <- any(sapply(koi8_r_codes, grepl, qp_encoded, fixed = TRUE))

  # ISO-8859-9 (Turkish)
  iso_8859_9_present <- any(sapply(iso_8859_9_codes, grepl, qp_encoded, fixed = TRUE))

  # Windows-1254 (Turkish)
  windows_1254_present <- any(sapply(windows_1254_codes, grepl, qp_encoded, fixed = TRUE))



  # qp_encoded <- tolower(qp_encoded)
  qp_encoded_split <- unlist(strsplit(qp_encoded, " "))
  # using split will be important when trying to decode a sentence with capital letters

  decoded_string <- stringi::stri_replace_all_fixed(qp_encoded_split, qp_before, qp_after, vectorize_all=FALSE)

  tryCatch({
    # adjustments based on the detection v1.1.4 bugfix
    if (utf8_present) {
      decoded_string <- iconv(decoded_string, from = "UTF-8", to = "UTF-8")
    } else if (latin1_present) {
      decoded_string <- iconv(decoded_string, from = "latin1", to = "UTF-8")
    } else if (iso_8859_10_present) {
      decoded_string <- iconv(decoded_string, from = "ISO-8859-10", to = "UTF-8")
    } else if (windows_1252_present) {
      decoded_string <- iconv(decoded_string, from = "windows-1252", to = "UTF-8")
    # } else if (iso_8859_5_present) {
      # decoded_string <- iconv(decoded_string, from = "ISO-8859-5", to = "UTF-8")
      # decoded_string <- iconv(decoded_string, from = "UTF-8", to = "UTF-8", sub="byte")
    # } else if (koi8_r_present) {
    #   decoded_string <- iconv(decoded_string, from = "KOI8-R", to = "UTF-8")
    } else if (iso_8859_9_present) {
      decoded_string <- iconv(decoded_string, from = "ISO-8859-9", to = "UTF-8")
    } else if (windows_1254_present) {
      decoded_string <- iconv(decoded_string, from = "windows-1254", to = "UTF-8")
    }

  }, error = function(e) {
    # Encoding(decoded_string)  <- "UTF-8"
    Encoding(decoded_string[grepl(pattern = "[\\x80-\\xff]", x = decoded_string, useBytes = TRUE)]) <- "UTF-8"

  })

  decoded_string <- paste0(decoded_string, collapse = " ")

  decoded_string <- gsub("_", " ", decoded_string, useBytes = TRUE) # recommendation https://tools.ietf.org/html/rfc2047#section-4, item 4.2
  # The "Q" encoding is similar to the "Quoted-Printable" content-
  #   transfer-encoding defined in RFC 2045.  It is designed to allow text
  # containing mostly ASCII characters to be decipherable on an ASCII
  # terminal without decoding.
  #
  # (1) Any 8-bit value may be represented by a "=" followed by two
  # hexadecimal digits.  For example, if the character set in use
  # were ISO-8859-1, the "=" character would thus be encoded as
  # "=3D", and a SPACE by "=20".  (Upper case should be used for
  #                                hexadecimal digits "A" through "F".)
  #
  # (2) The 8-bit hexadecimal value 20 (e.g., ISO-8859-1 SPACE) may be
  # represented as "_" (underscore, ASCII 95.).  (This character may
  # not pass through some internetwork mail gateways, but its use
  # will greatly enhance readability of "Q" encoded data with mail
  # readers that do not support this encoding.)  Note that the "_"
  # always represents hexadecimal 20, even if the SPACE character
  # occupies a different code position in the character set in use.
  #
  # (3) 8-bit values which correspond to printable ASCII characters other
  # than "=", "?", and "_" (underscore), MAY be represented as those
  # characters.  (But see section 5 for restrictions.)  In
  # particular, SPACE and TAB MUST NOT be represented as themselves
  # within encoded words.

  return(decoded_string)

}
