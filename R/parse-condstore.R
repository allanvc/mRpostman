#' Parse the MODIFIED response code of a conditional STORE (RFC 7162)
#' @param resp_char A \code{character} string with the server response.
#' @return An \code{integer} vector with the ids the server did NOT update
#'   (empty when every message was updated).
#' @noRd
parse_modified <- function(resp_char) {
  m <- stringr::str_match(resp_char, "\\[MODIFIED\\s+([0-9,:]+)\\]")
  if (is.na(m[1, 2])) return(integer(0))
  expand_sequence_set(m[1, 2])
}

#' Parse the HIGHESTMODSEQ response code of SELECT/EXAMINE
#' @noRd
parse_highestmodseq <- function(resp_char) {
  m <- stringr::str_match(resp_char, "\\[HIGHESTMODSEQ\\s+(\\d+)\\]")
  if (is.na(m[1, 2])) NA_real_ else as.numeric(m[1, 2])
}

#' Parse the changes reported by a QRESYNC SELECT or a VANISHED/CHANGEDSINCE FETCH
#'
#' Reads the untagged \code{* VANISHED [(EARLIER)] <uid-set>} lines and the
#' \code{* <n> FETCH (UID <u> FLAGS (...) MODSEQ (<m>))} lines (RFC 7162).
#' @param resp_char A \code{character} string with the server response.
#' @return A \code{list} with \code{vanished} (an integer vector of UIDs),
#'   \code{changed} (a \code{data.frame} with \code{seq}, \code{uid},
#'   \code{flags}, \code{modseq}), \code{highestmodseq}, \code{uidvalidity},
#'   \code{uidnext}, and \code{exists} (\code{NA} when not reported).
#' @noRd
parse_resync <- function(resp_char) {
  lines <- unique(strsplit(resp_char, "\r?\n")[[1]])
  vanished <- integer(0)
  for (ln in grep("^\\*\\s+VANISHED\\b", lines, value = TRUE)) {
    set <- stringr::str_match(ln, "VANISHED\\s+(?:\\(EARLIER\\)\\s+)?([0-9,:]+)")[1, 2]
    if (!is.na(set)) vanished <- c(vanished, expand_sequence_set(set))
  }
  fl <- grep("^\\*\\s+\\d+\\s+FETCH\\s+\\(", lines, value = TRUE)
  fm <- stringr::str_match(fl, "^\\*\\s+(\\d+)\\s+FETCH\\s+\\((.*)\\)\\s*$")
  changed <- data.frame(seq = integer(0), uid = integer(0), flags = character(0),
                        modseq = numeric(0), stringsAsFactors = FALSE)
  if (nrow(fm) > 0) {
    body <- fm[, 3]
    changed <- data.frame(
      seq    = as.integer(fm[, 2]),
      uid    = suppressWarnings(as.integer(stringr::str_match(body, "\\bUID\\s+(\\d+)")[, 2])),
      flags  = stringr::str_match(body, "\\bFLAGS\\s+\\(([^\\)]*)\\)")[, 2],
      modseq = suppressWarnings(as.numeric(stringr::str_match(body, "\\bMODSEQ\\s+\\((\\d+)\\)")[, 2])),
      stringsAsFactors = FALSE)
    changed <- changed[!duplicated(changed$seq), , drop = FALSE]
    rownames(changed) <- NULL
  }
  num <- function(key) { m <- stringr::str_match(resp_char, paste0("\\[", key, "\\s+(\\d+)\\]"))[1, 2]; if (is.na(m)) NA_real_ else as.numeric(m) }
  ex <- stringr::str_match(resp_char, "\\*\\s+(\\d+)\\s+EXISTS")[1, 2]
  list(vanished = unique(vanished), changed = changed,
       highestmodseq = num("HIGHESTMODSEQ"), uidvalidity = num("UIDVALIDITY"),
       uidnext = num("UIDNEXT"), exists = if (is.na(ex)) NA_real_ else as.numeric(ex))
}
