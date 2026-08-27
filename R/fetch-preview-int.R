#' Fetch the server-generated preview of messages (INTERNAL HELPER)
#'
#' Issues \code{FETCH <id> (PREVIEW)} (RFC 8970) and returns the short text
#' snippet the server generates for each message, without transferring its
#' body. Requires the server \code{PREVIEW} capability.
#' @noRd
fetch_preview_int <- function(self, msg_id, use_uid, retries) {

  check_args(msg_id = msg_id, use_uid = use_uid, retries = retries)

  assert_capability(self, "PREVIEW", command = "fetch_preview",
                    rfc = "RFC 8970", retries = retries)

  use_uid_string <- if (isTRUE(use_uid)) "UID " else NULL
  fetch_request <- paste0(use_uid_string, "FETCH ", "#", " (PREVIEW)")

  msg_list <- execute_fetch_loop(self = self, msg_id = msg_id,
                                 fetch_request = fetch_request,
                                 use_uid = use_uid, write_to_disk = FALSE,
                                 keep_in_mem = FALSE, retries = retries,
                                 fetch_type = "preview",
                                 metadata_attribute = "PREVIEW")

  out <- vapply(msg_list, parse_preview, character(1))
  names(out) <- sub("^preview", "", names(msg_list))
  out

}

#' Parse the PREVIEW item of a FETCH response
#' @param x The cleaned FETCH response of one message.
#' @return The preview text (\code{NA} when the server returned \code{NIL}).
#' @noRd
parse_preview <- function(x) {
  if (grepl("PREVIEW\\s+NIL", x)) {
    return(NA_character_)
  }
  m <- stringr::str_match(x, 'PREVIEW\\s+"((?:[^"\\\\]|\\\\.)*)"')
  if (is.na(m[1, 2])) {
    # literal form: PREVIEW {n}\r\n<text>
    m2 <- stringr::str_match(x, "PREVIEW\\s+\\{\\d+\\}\r?\n([^\r\n]*)")
    return(if (is.na(m2[1, 2])) NA_character_ else m2[1, 2])
  }
  gsub('\\\\(["\\\\])', "\\1", m[1, 2])
}
