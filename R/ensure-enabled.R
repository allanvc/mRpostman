#' Enable an extension on the current connection, once per connection (INTERNAL HELPER)
#'
#' \code{ENABLE} (RFC 5161) is accepted only in the authenticated, not-selected
#' state, and what it enables lives on the current TCP connection, which
#' libcurl may replace at any time (servers such as Gmail close the connection
#' after a rejected command). The connection epoch counted by the debug
#' callback tells whether the extension is still enabled; when it is not, the
#' selected folder is released with \code{UNSELECT}, the extension enabled,
#' and the folder selected again.
#' @param self The R6 connection object.
#' @param ext The extension name, e.g. \code{"UTF8=ACCEPT"} or \code{"QRESYNC"}.
#' @param caps The server capabilities (upper case).
#' @param retries Number of attempts to connect and execute the command.
#' @return \code{TRUE} when the extension is enabled on the current
#'   connection; \code{FALSE} when a folder is selected and the server lacks
#'   \code{UNSELECT}, so that it cannot be released without expunging.
#' @noRd
ensure_enabled <- function(self, ext, caps, retries = 1) {
  ext <- toupper(ext)
  epoch <- if (is.null(self$con_debug)) 0L else self$con_debug$epoch
  if (is.null(self$enabled_epochs)) {
    self$enabled_epochs <- list()
  }
  if (identical(self$enabled_epochs[[ext]], epoch)) {
    return(TRUE)
  }
  folder <- self$con_params$folder
  if (!is.na(folder)) {
    if (!("UNSELECT" %in% caps)) {
      return(FALSE)
    }
    unselect_folder_int(self, retries)
    self$con_params$folder <- NA
  }
  enable_int(self, ext, retries)
  # the ENABLE may itself have triggered a reconnection: record the epoch now
  self$enabled_epochs[[ext]] <- if (is.null(self$con_debug)) 0L else self$con_debug$epoch
  if (!is.na(folder)) {
    select_folder_int(self, name = folder, mute = TRUE, retries = 0)
    self$con_params$folder <- folder
  }
  TRUE
}
