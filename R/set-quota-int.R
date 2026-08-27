#' Set the resource limits of a quota root (INTERNAL HELPER)
#'
#' Issues \code{SETQUOTA <root> (<resource> <limit> ...)} (RFC 9208). Most
#' servers restrict this command to administrators; the server's answer is
#' returned as parsed by \code{parse_quota()}.
#' @param quota_root A string with the quota root name.
#' @param storage \code{NULL} or the new \code{STORAGE} limit, in kibibytes.
#' @param message \code{NULL} or the new \code{MESSAGE} limit (number of
#'   messages).
#' @param retries Number of attempts to connect and execute the command.
#' @noRd
set_quota_int <- function(self, quota_root, storage, message, retries) {

  assertthat::assert_that(
    is.character(quota_root),
    msg='"quota_root" must be of type character.')
  assertthat::assert_that(
    !is.null(storage) || !is.null(message),
    msg='at least one of "storage" and "message" must be given.')
  for (lim in list(storage, message)) {
    if (!is.null(lim)) {
      assertthat::assert_that(
        is.numeric(lim), length(lim) == 1, lim >= 0,
        msg='"storage" and "message" must be NULL or a single non-negative number.')
    }
  }

  check_args(retries = retries)

  assert_capability(self, "QUOTA", command = "set_quota", rfc = "RFC 9208",
                    retries = retries)

  limits <- c(if (!is.null(storage)) paste("STORAGE", format(storage, scientific = FALSE)),
              if (!is.null(message)) paste("MESSAGE", format(message, scientific = FALSE)))

  customrequest <- paste0("SETQUOTA ", adjust_folder_name(quota_root),
                          " (", paste(limits, collapse = " "), ")")

  resp_char <- execute_simple_command(self, customrequest, retries)
  parse_quota(resp_char)

}
