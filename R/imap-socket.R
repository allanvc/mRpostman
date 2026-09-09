#' @useDynLib mRpostman, .registration = TRUE
#' @noRd
NULL

# Thin wrappers over the C helpers of src/imap_socket.c: a raw (TLS) socket to
# an IMAP server, established by libcurl in CONNECT_ONLY mode.

# use_ssl = TRUE on an imap:// URL upgrades the connection with STARTTLS before
# the socket is handed over (imaps:// URLs are TLS from the start regardless)
imap_socket_open <- function(url, timeout_ms = 30000, verify_peer = TRUE, use_ssl = FALSE) {
  .Call(C_imap_socket_open, as.character(url), as.integer(timeout_ms),
        as.logical(verify_peer), as.logical(use_ssl))
}
imap_socket_send <- function(sock, data, timeout_ms = 30000) {
  if (is.character(data)) data <- charToRaw(data)
  .Call(C_imap_socket_send, sock, data, as.integer(timeout_ms))
}
imap_socket_recv <- function(sock, timeout_ms = 30000, max_bytes = 65536L) {
  .Call(C_imap_socket_recv, sock, as.integer(timeout_ms), as.integer(max_bytes))
}
imap_socket_close <- function(sock) .Call(C_imap_socket_close, sock)
imap_socket_is_open <- function(sock) .Call(C_imap_socket_is_open, sock)
