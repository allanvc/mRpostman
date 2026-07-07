#' Parse a QUOTA response into a data.frame of resource usage/limits
#'
#' Extracts the \code{* QUOTA <root> (<resource> <usage> <limit> ...)} responses
#' (RFC 2087) into one row per resource. \code{* QUOTAROOT} lines (returned by
#' \code{GETQUOTAROOT}) are ignored — only the actual quota figures are kept.
#' @param content_char A \code{character} string with the server response
#'   (headers and/or content).
#' @return A \code{data.frame} with columns \code{quota_root}, \code{resource},
#'   \code{usage}, and \code{limit} (one row per resource), possibly empty.
#' @noRd
parse_quota <- function(content_char) {

  lines <- strsplit(content_char, "\r\n")[[1]]

  qroot <- character(0)
  res <- character(0)
  usage <- numeric(0)
  limit <- numeric(0)

  for (ln in lines) {
    m <- stringr::str_match(ln, '^\\* QUOTA (?:"([^"]*)"|(\\S+)) \\((.*)\\)')
    if (is.na(m[1, 1])) {
      next
    }
    root <- if (!is.na(m[1, 2])) m[1, 2] else m[1, 3]
    body <- m[1, 4]

    # resource triples: <name> <usage> <limit>
    trip <- stringr::str_match_all(body, '(\\S+)\\s+(\\d+)\\s+(\\d+)')[[1]]
    if (nrow(trip) == 0) {
      next
    }
    for (i in seq_len(nrow(trip))) {
      qroot <- c(qroot, root)
      res <- c(res, trip[i, 2])
      usage <- c(usage, as.numeric(trip[i, 3]))
      limit <- c(limit, as.numeric(trip[i, 4]))
    }
  }

  data.frame(quota_root = qroot, resource = res,
             usage = usage, limit = limit, stringsAsFactors = FALSE)

}
