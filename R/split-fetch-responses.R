#' Split a multi-message FETCH response into one element per message
#'
#' A \code{FETCH} issued on a set of messages (e.g. the \code{"$"} reference of
#' a saved search, SEARCHRES) returns one untagged \code{* <n> FETCH (...)}
#' block per message in a single reply. This helper splits the reply at the
#' start of each block and cleans every block with
#' \code{clean_fetch_results()}, naming the elements by sequence number (or
#' UID, when the block carries a \code{UID} item and \code{use_uid} is
#' \code{TRUE}).
#' @param raw_text The raw server reply.
#' @param fetch_type The prefix used to name the elements (\code{"body"},
#'   \code{"header"}, \code{"text"}, \code{"metadata"}).
#' @param use_uid A logical; if \code{TRUE}, elements are named by UID.
#' @param metadata_attribute Passed on to \code{clean_fetch_results()}.
#' @return A named \code{list} of cleaned message parts.
#' @noRd
split_fetch_responses <- function(raw_text, fetch_type, use_uid = FALSE,
                                  metadata_attribute = NULL) {
  pieces <- strsplit(raw_text, "(?<=\r\n)(?=\\* \\d+ (?:UID)?FETCH )", perl = TRUE)[[1]]
  pieces <- pieces[grepl("^\\* \\d+ (?:UID)?FETCH ", pieces)]
  if (length(pieces) == 0) {
    return(list())
  }
  out <- vector("list", length(pieces))
  nm <- character(length(pieces))
  for (i in seq_along(pieces)) {
    seqno <- stringr::str_match(pieces[i], "^\\* (\\d+) (?:UID)?FETCH ")[1, 2]
    uid <- stringr::str_match(pieces[i], "^\\* \\d+ (?:UID)?FETCH \\([^\r\n]*?\\bUID (\\d+)")[1, 2]
    if (is.na(uid) && grepl("^\\* \\d+ UIDFETCH ", pieces[i])) {
      # UIDONLY (RFC 9586): the leading number of a UIDFETCH block is the UID
      uid <- seqno
    }
    id <- if (isTRUE(use_uid) && !is.na(uid)) uid else seqno
    nm[i] <- paste0(fetch_type, if (isTRUE(use_uid)) "UID" else "", id)
    # every block but the last ends with its own ")\r\n" (the last one is
    # followed by the tagged line, which clean_fetch_results() strips)
    piece <- sub("\\)\r\n$", "", pieces[i])
    out[[i]] <- clean_fetch_results(piece, metadata_attribute)
  }
  names(out) <- nm
  out
}
