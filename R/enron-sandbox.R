#' @title Populate a Mailbox with a Subset of the Enron Corpus
#' @description Downloads the public Enron e-mail corpus (CMU, May 7 2015
#'   release; ~423 MB, one time, cached locally), selects a subset by
#'   custodian, folder, and date, and ingests it into the connected IMAP
#'   server through \code{\link{ingest_maildir}} -- i.e., through the
#'   package's own \code{APPEND} implementation. One server folder is
#'   created per custodian. It is meant to be run against the disposable
#'   local Docker IMAP server shipped in
#'   \code{system.file("docker", package = "mRpostman")}, providing real
#'   data for demonstrations and teaching on top of the synthetic corpus of
#'   \code{\link{sandbox_corpus}}.
#' @param con An \code{ImapCon} object, as returned by
#'   \code{\link{configure_imap}}.
#' @param custodians A character vector of custodian directory names, e.g.
#'   \code{c("kaminski-v", "lay-k", "skilling-j")} (the default). The 150
#'   available names are the directories of the corpus' \code{maildir/}
#'   root.
#' @param folder_pattern A regular expression matched against the
#'   custodians' folder names. The default,
#'   \code{"^_?sent(_items|_mail)?$"}, selects the sent-mail folder
#'   variants. Use \code{".*"} for all folders of the selected custodians.
#' @param sent_since,sent_before Optional \code{Date} (or
#'   \code{"YYYY-MM-DD"} string) limits applied to each message's
#'   \code{Date:} header before ingestion. \code{NULL} (default) applies no
#'   limit.
#' @param max_msgs Maximum number of messages per custodian, applied after
#'   the other filters. Default is \code{Inf}.
#' @param cache_dir Directory where the corpus tarball and its extraction
#'   are cached across sessions. Default is
#'   \code{tools::R_user_dir("mRpostman", "cache")}.
#' @param url URL of the corpus tarball. Default is the CMU distribution
#'   point.
#' @param ask A \code{logical}. If \code{TRUE} (default), asks for
#'   confirmation before the one-time ~423 MB download (~1.4 GB extracted).
#'   Set to \code{FALSE} for non-interactive use.
#' @param mute A \code{logical}. Passed to \code{\link{ingest_maildir}}.
#'   Default is \code{FALSE}.
#' @return Invisibly, a \code{data.frame} with one row per ingested message:
#'   \code{custodian}, \code{path} (relative to the corpus root),
#'   \code{size}, \code{date} (parsed from the header; \code{NA} when
#'   unparseable), and \code{appended}.
#' @details The corpus was released into the public domain by the U.S.
#'   Federal Energy Regulatory Commission and is curated at Carnegie Mellon
#'   University (\url{https://www.cs.cmu.edu/~enron/}); it contains about
#'   517,000 messages from 150 custodians in maildir layout, without
#'   attachments. Nothing is downloaded when the cache is already in place.
#'   If the download fails (offline use, moved URL), the function fails
#'   gracefully with an informative message and leaves the cache untouched.
#'   Date filtering parses each candidate file's \code{Date:} header
#'   locale-independently; messages without a parseable date are kept unless
#'   both limits are set.
#' @family sandbox
#' @export
#' @examples
#' \dontrun{
#' con <- configure_imap(url = "imap://localhost:1430",
#'                       username = "enron", password = "sandbox",
#'                       use_ssl = FALSE)
#' # sent mail of three central custodians, 2000-2002
#' manifest <- enron_sandbox(con,
#'                           custodians = c("kaminski-v", "lay-k",
#'                                          "skilling-j"),
#'                           sent_since  = "2000-01-01",
#'                           sent_before = "2002-07-01")
#' table(manifest$custodian, manifest$appended)
#' }
enron_sandbox <- function(con,
                          custodians = c("kaminski-v", "lay-k", "skilling-j"),
                          folder_pattern = "^_?sent(_items|_mail)?$",
                          sent_since = NULL, sent_before = NULL,
                          max_msgs = Inf,
                          cache_dir = tools::R_user_dir("mRpostman", "cache"),
                          url = "https://www.cs.cmu.edu/~enron/enron_mail_20150507.tar.gz",
                          ask = TRUE, mute = FALSE) {

  assertthat::assert_that(
    inherits(con, "ImapCon"),
    msg = '"con" must be an ImapCon object, as returned by configure_imap().')
  assertthat::assert_that(
    is.character(custodians), length(custodians) >= 1,
    msg = '"custodians" must be a character vector of custodian names.')

  maildir <- file.path(cache_dir, "maildir")
  tarball <- file.path(cache_dir, basename(url))

  ## ---- one-time download + extraction (cached, consented) -----------------
  if (!dir.exists(maildir)) {
    if (!file.exists(tarball)) {
      if (isTRUE(ask)) {
        if (!interactive()) {
          stop("the Enron corpus (~423 MB download, ~1.4 GB extracted) is ",
               "not cached yet and consent cannot be asked in a ",
               "non-interactive session. Re-run with `ask = FALSE` to ",
               "authorize the one-time download to\n  ", cache_dir,
               call. = FALSE)
        }
        ans <- readline(paste0(
          "Download the Enron corpus (~423 MB, extracts to ~1.4 GB) to\n  ",
          cache_dir, " ? [y/N] "))
        if (!tolower(substr(ans, 1, 1)) %in% "y") {
          stop("download not authorized.", call. = FALSE)
        }
      }
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
      old_timeout <- getOption("timeout"); on.exit(options(timeout = old_timeout))
      options(timeout = max(3600, old_timeout))
      status <- tryCatch(
        utils::download.file(url, tarball, mode = "wb", quiet = mute),
        error = function(e) e, warning = function(w) w)
      if (inherits(status, "condition") ||
          (is.numeric(status) && status != 0) || !file.exists(tarball)) {
        unlink(tarball)
        stop("could not download the Enron corpus from\n  ", url,
             "\nCheck your connection, or download the tarball manually ",
             "into\n  ", cache_dir, call. = FALSE)
      }
    }
    if (!mute) cat("::mRpostman: extracting the corpus (one time) ...\n")
    utils::untar(tarball, exdir = cache_dir)
  }

  ## ---- subset selection ----------------------------------------------------
  since  <- if (is.null(sent_since))  NULL else as.Date(sent_since)
  before <- if (is.null(sent_before)) NULL else as.Date(sent_before)

  out <- vector("list", length(custodians))
  for (k in seq_along(custodians)) {
    cust <- custodians[k]
    base <- file.path(maildir, cust)
    assertthat::assert_that(
      dir.exists(base),
      msg = paste0('custodian "', cust, '" not found in the corpus; the 150 ',
                   'valid names are the directories under\n  ', maildir))

    dirs <- list.dirs(base, recursive = FALSE, full.names = TRUE)
    sel  <- dirs[grepl(folder_pattern, basename(dirs))]
    files <- sort(list.files(sel, full.names = TRUE, recursive = TRUE))

    dates <- vapply(files, .enron_header_date, character(1))
    dates <- as.Date(dates)
    keep  <- rep(TRUE, length(files))
    if (!is.null(since))  keep <- keep & (is.na(dates) | dates >= since)
    if (!is.null(before)) keep <- keep & (is.na(dates) | dates <  before)
    if (!is.null(since) && !is.null(before)) keep <- keep & !is.na(dates)
    files <- files[keep]; dates <- dates[keep]
    if (is.finite(max_msgs) && length(files) > max_msgs) {
      files <- files[seq_len(max_msgs)]; dates <- dates[seq_len(max_msgs)]
    }

    if (!mute) {
      cat("::mRpostman:", cust, "-", length(files), "messages selected\n")
    }
    man <- ingest_maildir(con, folder = cust, files = files, mute = TRUE)
    out[[k]] <- data.frame(
      custodian = cust,
      path = sub(paste0("^", maildir, .Platform$file.sep), "", man$path),
      size = man$size,
      date = dates,
      appended = man$appended,
      row.names = NULL,
      stringsAsFactors = FALSE)
    if (!mute) {
      cat("::mRpostman:", cust, "-", sum(man$appended), "messages ingested\n")
    }
  }

  invisible(do.call(rbind, out))
}

# parse the Date: header of one message file, locale-independently;
# returns "YYYY-MM-DD" or NA_character_. All matching is done byte-wise:
# real-world corpora contain headers with 8-bit bytes that are invalid in the
# session encoding, and encoding-aware regexprs warn and fail on them.
.enron_header_date <- function(file) {
  head_bytes <- readChar(file, min(4096L, file.size(file)), useBytes = TRUE)
  m <- regmatches(head_bytes,
                  regexpr("(^|\r?\n)Date:[^\r\n]*", head_bytes,
                          useBytes = TRUE))
  if (length(m) == 0) return(NA_character_)
  d <- sub("^(\r?\n)?Date:[[:space:]]*", "", m[1], useBytes = TRUE)
  # e.g. "Mon, 14 May 2001 16:39:00 -0700 (PDT)" or "14 May 2001 16:39:00 -0700"
  r <- regmatches(d, regexpr(
    "([0-9]{1,2})[[:space:]]+(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[[:space:]]+([0-9]{4})",
    d, useBytes = TRUE))
  if (length(r) == 0) return(NA_character_)
  parts <- strsplit(trimws(r[1]), "[[:space:]]+")[[1]]
  months <- c(Jan = 1, Feb = 2, Mar = 3, Apr = 4, May = 5, Jun = 6,
              Jul = 7, Aug = 8, Sep = 9, Oct = 10, Nov = 11, Dec = 12)
  sprintf("%s-%02d-%02d", parts[3], months[[parts[2]]], as.integer(parts[1]))
}
