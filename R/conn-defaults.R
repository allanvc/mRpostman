# Connection-level defaults (since the 2026 refactoring): use_uid, mute, and
# retries can be set once in configure_imap() and are used by every method
# whose corresponding argument is left NULL. Passing a value per call still
# overrides the connection-level setting.

#' Resolve an argument against its connection-level default (INTERNAL)
#' @noRd
conn_default <- function(value, self, name, fallback) {
  if (!is.null(value)) {
    return(value)
  }
  v <- tryCatch(self$con_params[[name]], error = function(e) NULL)
  if (is.null(v)) fallback else v
}
