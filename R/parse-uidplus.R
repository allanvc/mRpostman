#' Expand an IMAP sequence-set into an integer vector
#'
#' Expands a sequence-set such as \code{"1:3,5,9:11"} (RFC 3501 section 9)
#' into \code{c(1, 2, 3, 5, 9, 10, 11)}, without evaluating server-provided
#' text as R code. Used by the UIDPLUS response-code parsers.
#' @param seqset A string with an IMAP sequence-set.
#' @return An \code{integer} vector (\code{integer(0)} for an empty or invalid
#'   set).
#' @noRd
expand_sequence_set <- function(seqset) {
  ids <- integer(0)
  if (is.na(seqset) || !nzchar(seqset)) {
    return(ids)
  }
  for (tok in strsplit(seqset, ",", fixed = TRUE)[[1]]) {
    if (grepl(":", tok, fixed = TRUE)) {
      b <- suppressWarnings(as.integer(strsplit(tok, ":", fixed = TRUE)[[1]]))
      if (length(b) == 2 && !anyNA(b)) {
        ids <- c(ids, seq.int(b[1], b[2]))
      }
    } else {
      n <- suppressWarnings(as.integer(tok))
      if (!is.na(n)) {
        ids <- c(ids, n)
      }
    }
  }
  ids
}

#' Parse the APPENDUID response code of an APPEND command (UIDPLUS, RFC 4315)
#'
#' Servers that advertise \code{UIDPLUS} answer a successful \code{APPEND} with
#' a tagged \code{OK [APPENDUID <uidvalidity> <uid>]} line. This helper reads
#' that response code.
#' @param resp_char A \code{character} string with the server response
#'   (headers and content pasted together).
#' @return A named \code{integer} vector \code{c(uidvalidity = , uid = )}, or
#'   \code{NULL} when the response carries no \code{APPENDUID} code.
#' @noRd
parse_appenduid <- function(resp_char) {
  m <- stringr::str_match(resp_char, "\\[APPENDUID\\s+(\\d+)\\s+(\\d+)\\]")
  if (is.na(m[1, 1])) {
    return(NULL)
  }
  c(uidvalidity = as.integer(m[1, 2]), uid = as.integer(m[1, 3]))
}

#' Parse the COPYUID response code of a COPY/MOVE command (UIDPLUS, RFC 4315)
#'
#' Servers that advertise \code{UIDPLUS} answer a successful \code{COPY} or
#' \code{MOVE} with an \code{OK [COPYUID <uidvalidity> <source-set> <dest-set>]}
#' response code, in which the i-th UID of the source set was copied to the
#' i-th UID of the destination set.
#' @param resp_char A \code{character} string with the server response
#'   (headers and content pasted together).
#' @return A \code{data.frame} with columns \code{source_uid} and
#'   \code{dest_uid} and the attribute \code{"uidvalidity"} (of the
#'   destination folder), or \code{NULL} when the response carries no
#'   \code{COPYUID} code.
#' @noRd
parse_copyuid <- function(resp_char) {
  m <- stringr::str_match(resp_char,
                          "\\[COPYUID\\s+(\\d+)\\s+([0-9,:]+)\\s+([0-9,:]+)\\]")
  if (is.na(m[1, 1])) {
    return(NULL)
  }
  src <- expand_sequence_set(m[1, 3])
  dst <- expand_sequence_set(m[1, 4])
  if (length(src) != length(dst)) {
    return(NULL) # malformed: the sets must be of equal size (RFC 4315, 3.)
  }
  out <- data.frame(source_uid = src, dest_uid = dst)
  attr(out, "uidvalidity") <- as.integer(m[1, 2])
  out
}
