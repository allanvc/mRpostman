#' Parse a LIST response into one row per folder with its attributes
#'
#' Reads the untagged \code{* LIST (<attributes>) "<delimiter>" <name>} lines
#' of a \code{LIST ... RETURN (CHILDREN SPECIAL-USE SUBSCRIBED)} response
#' (LIST-EXTENDED, RFC 5258).
#' @param resp_char A \code{character} string with the server response.
#' @return A \code{data.frame} with columns \code{folder}, \code{delimiter},
#'   \code{attributes} (the attribute list as one string), \code{selectable},
#'   \code{has_children}, \code{subscribed}, and \code{special_use} (the
#'   special-use attribute, or \code{NA}).
#' @noRd
parse_list_extended <- function(resp_char, my_rights = FALSE) {
  lines <- strsplit(resp_char, "\r?\n")[[1]]
  list_lines <- unique(grep("^\\*\\s+LIST\\s+\\(", lines, value = TRUE))
  lm <- stringr::str_match(list_lines,
                           "^\\*\\s+LIST\\s+\\(([^\\)]*)\\)\\s+(?:\"([^\"]*)\"|(NIL))\\s+(?:\"(.*)\"|(\\S+))\\s*$")
  ok <- !is.na(lm[, 1])
  lm <- lm[ok, , drop = FALSE]
  attrs <- lm[, 2]
  special <- stringr::str_extract(attrs, "\\\\(Sent|Drafts|Junk|Trash|Archive|All|Flagged|Important)\\b")
  out <- data.frame(
    folder = imap_utf7_decode(ifelse(is.na(lm[, 5]), lm[, 6], lm[, 5])),
    delimiter = ifelse(is.na(lm[, 3]), NA_character_, lm[, 3]),
    attributes = attrs,
    selectable = !grepl("\\\\Noselect|\\\\NonExistent", attrs, ignore.case = TRUE),
    has_children = grepl("\\\\HasChildren", attrs, ignore.case = TRUE),
    subscribed = grepl("\\\\Subscribed", attrs, ignore.case = TRUE),
    special_use = special,
    stringsAsFactors = FALSE)
  if (isTRUE(my_rights)) {
    # "* MYRIGHTS <folder> <rights>" lines (LIST-MYRIGHTS, RFC 8440)
    rl <- unique(grep("^\\*\\s+MYRIGHTS\\s", lines, value = TRUE))
    rm_ <- stringr::str_match(rl, "^\\*\\s+MYRIGHTS\\s+(?:\"(.*)\"|(\\S+))\\s+(\\S+)")
    rnames <- imap_utf7_decode(ifelse(is.na(rm_[, 2]), rm_[, 3], rm_[, 2]))
    out$my_rights <- rm_[match(out$folder, rnames), 4]
  }
  out
}
