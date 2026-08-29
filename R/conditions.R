# Classed conditions for the package. Every user-facing failure is signaled
# as an "mRpostman_error" with a specific subclass, so callers can program
# against failure kinds instead of matching message text:
#
#   tryCatch(con$search(...),
#            mRpostman_capability_error = function(e) ...,
#            mRpostman_server_error     = function(e) e$server_reply)
#
# Subclasses:
#   mRpostman_connection_error   - the transport is unusable (dead handle,
#                                  resolve failure, authentication)
#   mRpostman_state_error        - a precondition on the session is missing
#                                  (e.g. no folder selected); retryable after
#                                  fixing the state
#   mRpostman_capability_error   - the server does not advertise a required
#                                  capability
#   mRpostman_server_error       - the server rejected the command (NO/BAD);
#                                  carries the server's reason in $server_reply
#   mRpostman_response_too_large - one response line exceeded libcurl's buffer

#' Signal a classed mRpostman error (INTERNAL HELPER)
#' @param message The error message.
#' @param class The specific subclass, e.g. "server_error".
#' @param ... Extra fields stored on the condition (e.g. server_reply).
#' @noRd
stop_mrp <- function(message, class, ...) {
  cond <- structure(
    class = c(paste0("mRpostman_", class), "mRpostman_error",
              "error", "condition"),
    list(message = message, call = NULL, ...))
  stop(cond)
}

#' @noRd
stop_dead_handle <- function() {
  stop_mrp(paste0("The connection handle is dead. Please, configure a new ",
                  "IMAP connection with configure_imap()."),
           "connection_error")
}

#' @noRd
stop_no_folder <- function() {
  stop_mrp("No folder previously selected.", "state_error")
}

#' @noRd
stop_server_rejected <- function(server_reply, command = NULL) {
  stop_mrp(paste0("The server rejected the command",
                  if (!is.null(command)) paste0(" (", command, ")") else "",
                  ": ", server_reply),
           "server_error", server_reply = server_reply, command = command)
}

#' @noRd
stop_request_failed <- function(self = NULL, command = NULL) {
  reply <- last_server_error(
    if (!is.null(self) && !is.null(self$con_debug)) self$con_debug$lines)
  if (!is.na(reply)) {
    stop_server_rejected(reply, command)
  }
  stop_mrp(paste0("Request error: the server returned an error",
                  if (!is.null(command)) paste0(" (", command, ")") else "",
                  ". Try to increase \"timeout_ms\" or \"retries\"."),
           "connection_error", command = command)
}
