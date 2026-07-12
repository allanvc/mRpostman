# populate_mailbox.R
#
# Thin command-line wrapper around mRpostman::populate_sandbox(): builds the
# deterministic synthetic corpus of sandbox_corpus() and stores it in an IMAP
# mailbox using the package's own APPEND. The defaults target the local Docker
# sandbox described in this folder's README.md:
#
#   docker build -t mrpostman-sandbox .
#   docker run -d --name mrpostman-sandbox -p 1430:143 mrpostman-sandbox
#
# Usage from the shell:   Rscript populate_mailbox.R [n_messages]
# Usage from R:           n_messages <- 200; source("populate_mailbox.R")

library(mRpostman)

url      <- Sys.getenv("MRP_SANDBOX_URL", "imap://localhost:1430")
username <- Sys.getenv("MRP_SANDBOX_USER", "testuser")
password <- Sys.getenv("MRP_SANDBOX_PASS", "sandbox")

cmd_args <- commandArgs(trailingOnly = TRUE)
if (!exists("n_messages")) {
  n_messages <- if (length(cmd_args) >= 1) as.integer(cmd_args[1]) else 200L
}

con <- configure_imap(url = url, username = username, password = password,
                      use_ssl = FALSE)

info <- populate_sandbox(con, n = n_messages)
