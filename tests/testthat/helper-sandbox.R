# One probe for the Docker sandbox (inst/docker), shared by every live test
# file. The historical per-file names are kept as aliases.
sandbox_up <- function() {
  tryCatch({
    s <- mRpostman:::imap_socket_open("imap://localhost:1430", 2000, FALSE)
    mRpostman:::imap_socket_close(s)
    TRUE
  }, error = function(e) FALSE)
}
rare_sandbox_up <- sandbox_up
rare_sandbox_up2 <- sandbox_up
att_sandbox_up <- sandbox_up
