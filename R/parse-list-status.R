#' Parse a LIST ... RETURN (STATUS ...) response (LIST-STATUS, RFC 5819)
#'
#' Reads the untagged \code{* LIST} and \code{* STATUS} lines of a
#' \code{LIST "" "*" RETURN (STATUS (...))} response and joins them into one
#' row per folder. Folders flagged \code{\\Noselect} (or \code{\\NonExistent})
#' carry no \code{STATUS} line and get \code{NA} counts.
#' @param resp_char A \code{character} string with the server response
#'   (headers and content pasted together).
#' @param items The \code{STATUS} data items that were requested; they become
#'   the numeric columns of the result, in this order.
#' @return A \code{data.frame} with the column \code{folder} followed by one
#'   numeric column per requested item.
#' @noRd
parse_list_status <- function(resp_char, items) {

  items <- toupper(items)
  lines <- strsplit(resp_char, "\r?\n")[[1]]

  # "* LIST (<attributes>) "<sep>" <name>"; the name may be quoted or not
  list_lines <- grep("^\\*\\s+LIST\\s+\\(", lines, value = TRUE)
  lm <- stringr::str_match(list_lines,
                           "^\\*\\s+LIST\\s+\\(([^\\)]*)\\)\\s+(?:\"[^\"]*\"|NIL)\\s+(?:\"(.*)\"|(\\S+))\\s*$")
  folders <- ifelse(is.na(lm[, 3]), lm[, 4], lm[, 3])
  attrs <- lm[, 2]
  keep <- !is.na(folders) & !grepl("\\\\Noselect|\\\\NonExistent", attrs,
                                    ignore.case = TRUE)
  folders <- folders[keep]
  # libcurl may deliver the untagged lines through both the header and the
  # body callbacks; keep the first occurrence of each folder
  folders <- imap_utf7_decode(folders[!duplicated(folders)])

  out <- data.frame(folder = folders, stringsAsFactors = FALSE)
  for (it in items) {
    out[[it]] <- rep(NA_real_, length(folders))
  }

  # "* STATUS <name> (KEY value ...)"; one line per folder
  status_lines <- grep("^\\*\\s+STATUS\\s", lines, value = TRUE)
  sm <- stringr::str_match(status_lines,
                           "^\\*\\s+STATUS\\s+(?:\"(.*)\"|(\\S+))\\s+\\(([^\\)]*)\\)")
  for (i in seq_len(nrow(sm))) {
    name <- imap_utf7_decode(ifelse(is.na(sm[i, 2]), sm[i, 3], sm[i, 2]))
    row <- match(name, out$folder)
    if (is.na(row)) {
      next
    }
    pairs <- stringr::str_match_all(sm[i, 4], "([A-Za-z]+)\\s+(\\d+)")[[1]]
    for (j in seq_len(nrow(pairs))) {
      key <- toupper(pairs[j, 2])
      if (key %in% items) {
        out[row, key] <- as.numeric(pairs[j, 3])
      }
    }
  }

  rownames(out) <- NULL
  out
}
