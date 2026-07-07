#' Parse the special-use attributes from a LIST (SPECIAL-USE) response
#'
#' Extracts, for each \code{* LIST (...) "<sep>" "<folder>"} line (RFC 6154),
#' the folder name and any special-use attribute it carries
#' (\code{\\All}, \code{\\Archive}, \code{\\Drafts}, \code{\\Flagged},
#' \code{\\Junk}, \code{\\Sent}, \code{\\Trash}). Folders without a special-use
#' attribute are skipped.
#' @param content_char A \code{character} string with the server response body
#'   (typically \code{rawToChar(response$content)}).
#' @return A \code{data.frame} with columns \code{folder} and \code{special_use}
#'   (one row per folder/attribute), possibly empty.
#' @noRd
parse_special_use <- function(content_char) {

  su_flags <- c("\\All", "\\Archive", "\\Drafts", "\\Flagged",
                "\\Junk", "\\Sent", "\\Trash")

  lines <- strsplit(content_char, "\r\n")[[1]]

  folder <- character(0)
  special <- character(0)

  for (ln in lines) {
    if (!grepl("^\\* LIST", ln)) {
      next
    }
    attrs <- regmatches(ln, regexpr("\\([^)]*\\)", ln))
    if (length(attrs) == 0) {
      next
    }
    present <- su_flags[vapply(su_flags,
                               function(f) grepl(f, attrs, fixed = TRUE),
                               logical(1))]
    if (length(present) == 0) {
      next
    }
    # folder name is the last quoted token on the line
    quoted <- regmatches(ln, gregexpr('"[^"]*"', ln))[[1]]
    if (length(quoted) == 0) {
      next
    }
    nm <- gsub('"', '', quoted[length(quoted)])
    for (p in present) {
      folder <- c(folder, nm)
      special <- c(special, p)
    }
  }

  data.frame(folder = folder, special_use = special, stringsAsFactors = FALSE)

}
