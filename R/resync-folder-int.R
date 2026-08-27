#' Select a folder with QRESYNC and report what changed (INTERNAL HELPER)
#'
#' Issues \code{SELECT <folder> (QRESYNC (<uidvalidity> <modseq>))} (RFC 7162)
#' after enabling \code{QRESYNC} on the connection. The server answers with
#' the UIDs expunged since \code{modseq} (\code{VANISHED (EARLIER)}) and the
#' flags of the messages changed since then.
#' @noRd
resync_folder_int <- function(self, name, uidvalidity, modseq, retries) {
  check_args(name = name, retries = retries)
  for (v in list(uidvalidity, modseq)) {
    assertthat::assert_that(is.numeric(v), length(v) == 1, v >= 0,
                            msg='"uidvalidity" and "modseq" must be single non-negative numbers.')
  }
  caps <- toupper(get_server_capabilities(self, retries = retries))
  assert_capability(self, "QRESYNC", command = "resync_folder", rfc = "RFC 7162",
                    retries = retries)
  # ENABLE needs the not-selected state; release the current folder first
  if (!is.na(self$con_params$folder)) {
    assert_capability(self, "UNSELECT", command = "resync_folder (releasing the selected folder)",
                      rfc = "RFC 3691", retries = retries)
  }
  ensure_enabled(self, "QRESYNC", caps, retries)
  self$con_params$folder <- NA
  customrequest <- paste0("SELECT ", adjust_folder_name(name), " (QRESYNC (",
                          format(uidvalidity, scientific = FALSE),
                          " ", format(modseq, scientific = FALSE), "))")
  resp_char <- execute_simple_command(self, customrequest, retries)
  self$con_params$folder <- name
  out <- parse_resync(resp_char)
  self$con_params$highestmodseq <- out$highestmodseq
  out
}

#' Fetch the flag changes and expunges since a modification sequence (INTERNAL HELPER)
#'
#' Issues \code{UID FETCH 1:* (FLAGS MODSEQ) (CHANGEDSINCE <modseq> [VANISHED])}
#' on the selected folder (RFC 7162). \code{VANISHED} requires \code{QRESYNC}
#' to be enabled on the connection; without it only the changed flags are
#' reported.
#' @noRd
fetch_changes_int <- function(self, modseq, vanished, retries) {
  assertthat::assert_that(!is.na(self$con_params$folder), msg='No folder previously selected.')
  assertthat::assert_that(is.numeric(modseq), length(modseq) == 1, modseq >= 0,
                          msg='"modseq" must be a single non-negative number.')
  assertthat::assert_that(is.logical(vanished), length(vanished) == 1,
                          msg='"vanished" must be a logical.')
  check_args(retries = retries)
  caps <- toupper(get_server_capabilities(self, retries = retries))
  assert_capability(self, "CONDSTORE", command = "fetch_changes", rfc = "RFC 7162",
                    retries = retries)
  mods <- paste0("CHANGEDSINCE ", format(modseq, scientific = FALSE))
  if (isTRUE(vanished)) {
    assert_capability(self, "QRESYNC", command = "fetch_changes(vanished = TRUE)",
                      rfc = "RFC 7162", retries = retries)
    if (!isTRUE(ensure_enabled(self, "QRESYNC", caps, retries))) {
      stop("QRESYNC cannot be enabled while a folder is selected on a server without UNSELECT.", call. = FALSE)
    }
    mods <- paste0(mods, " VANISHED")
  }
  resp_char <- execute_simple_command(
    self, paste0("UID FETCH 1:* (FLAGS MODSEQ) (", mods, ")"), retries)
  out <- parse_resync(resp_char)
  out[c("vanished", "changed")]
}
