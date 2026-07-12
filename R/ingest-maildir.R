#' @title Ingest a Local Maildir into an IMAP Folder
#' @description Uploads every message file found under a local directory to a
#'   folder on the connected IMAP server, using the package's own
#'   \code{APPEND} implementation (\code{ImapCon$append_msg()}). Message files
#'   are read as raw bytes and appended verbatim, so any RFC 5322-conformant
#'   maildir-style layout (one message per file) can be ingested -- mail
#'   server backups, exported archives, or public corpora such as the Enron
#'   corpus (see \code{\link{enron_sandbox}}).
#' @param con An \code{ImapCon} object, as returned by
#'   \code{\link{configure_imap}}.
#' @param path A string with the path to a local directory containing message
#'   files.
#' @param folder A string with the name of the destination folder on the
#'   server. Default is \code{basename(path)}.
#' @param create A \code{logical}. If \code{TRUE} (default), the destination
#'   folder is created with \code{CREATE} when it does not exist yet.
#' @param recursive A \code{logical}. If \code{TRUE} (default), message files
#'   are searched recursively under \code{path}.
#' @param files An optional character vector of message file paths to ingest,
#'   overriding the directory listing. When provided, \code{path} and
#'   \code{recursive} are ignored for file discovery (a subset selected by
#'   the caller -- see \code{\link{enron_sandbox}} -- is uploaded as given).
#'   Default is \code{NULL}.
#' @param mute A \code{logical}. If \code{FALSE}, prints progress every 500
#'   messages and a final summary. Default is \code{FALSE}.
#' @param retries Number of attempts for each \code{APPEND}. Default is
#'   \code{1}.
#' @return Invisibly, a \code{data.frame} with one row per message file:
#'   \code{path}, \code{size}, and \code{appended} (a \code{logical}
#'   indicating whether the upload succeeded).
#' @details \code{APPEND} via libcurl stores messages with the \code{\\Seen}
#'   flag and the current internal date (see \code{\link{populate_sandbox}}
#'   for the implications); header-based \code{SENT*} searches are therefore
#'   the appropriate way to query ingested corpora by date. Files that fail
#'   to upload (e.g., malformed messages rejected by the server) are skipped
#'   with a warning rather than aborting the ingestion.
#' @family sandbox
#' @export
#' @examples
#' \dontrun{
#' con <- configure_imap(url = "imap://localhost:1430",
#'                       username = "testuser", password = "sandbox",
#'                       use_ssl = FALSE)
#' # upload a locally stored maildir folder to the server
#' manifest <- ingest_maildir(con, path = "~/backup/maildir/archive2020",
#'                            folder = "archive2020")
#' table(manifest$appended)
#' }
ingest_maildir <- function(con, path = NULL, folder = basename(path),
                           create = TRUE, recursive = TRUE, files = NULL,
                           mute = FALSE, retries = 1) {

  assertthat::assert_that(
    inherits(con, "ImapCon"),
    msg = '"con" must be an ImapCon object, as returned by configure_imap().')
  assertthat::assert_that(
    !is.null(path) || !is.null(files),
    msg = 'either "path" or "files" must be provided.')
  if (is.null(files)) {
    assertthat::assert_that(
      is.character(path), length(path) == 1, dir.exists(path),
      msg = '"path" must be the path to an existing local directory.')
  }
  assertthat::assert_that(
    is.character(folder), length(folder) == 1, nchar(folder) > 0,
    msg = '"folder" must be a non-empty string.')

  if (is.null(files)) {
    files <- sort(list.files(path, full.names = TRUE, recursive = recursive))
    files <- files[!dir.exists(files)]
  } else {
    missing_files <- files[!file.exists(files)]
    assertthat::assert_that(
      length(missing_files) == 0,
      msg = paste0('files not found: ',
                   paste(utils::head(missing_files, 3), collapse = ", "),
                   if (length(missing_files) > 3) ", ..." else ""))
  }
  assertthat::assert_that(
    length(files) > 0,
    msg = 'no message files to ingest.')

  if (isTRUE(create)) {
    tryCatch(con$create_folder(folder, mute = TRUE),
             error = function(e) invisible(NULL)) # folder may already exist
  }

  appended <- logical(length(files))
  for (i in seq_along(files)) {
    raw_msg <- readChar(files[i], file.size(files[i]), useBytes = TRUE)
    appended[i] <- tryCatch({
      con$append_msg(message = raw_msg, folder = folder, mute = TRUE,
                     retries = retries)
      TRUE
    }, error = function(e) {
      warning('skipping "', files[i], '": ', conditionMessage(e),
              call. = FALSE)
      FALSE
    })
    if (!mute && i %% 500 == 0) {
      cat("::mRpostman:", i, "of", length(files), "messages appended to",
          folder, "\n")
    }
  }

  if (!mute) {
    cat("::mRpostman:", sum(appended), "of", length(files),
        "messages ingested into", folder, "\n")
  }

  invisible(data.frame(path = files,
                       size = file.size(files),
                       appended = appended,
                       stringsAsFactors = FALSE))
}
