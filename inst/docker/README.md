# mRpostman IMAP sandbox

A disposable local IMAP server (Dovecot on Alpine Linux) for testing and
demonstrating `mRpostman` without a real mail account. Everything runs
offline, and the mailbox is populated deterministically by the package itself,
so results are reproducible across machines.

**Not suitable for production use**: no TLS, plaintext authentication, and a
single static password (`sandbox`) shared by every username.

## Requirements

- [Docker](https://docs.docker.com/get-docker/)
- `mRpostman` >= 1.3.0

## Quick start

From this folder (`system.file("docker", package = "mRpostman")`):

```sh
docker build -t mrpostman-sandbox .
docker run -d --name mrpostman-sandbox -p 1430:143 mrpostman-sandbox
```

Populate the mailbox with 200 synthetic messages (any username works; the
password is always `sandbox`):

```sh
Rscript populate_mailbox.R 200
```

or, from R:

```r
library(mRpostman)
con <- configure_imap(url = "imap://localhost:1430",
                      username = "testuser",
                      password = "sandbox",
                      use_ssl = FALSE)
populate_sandbox(con, n = 200)
```

Then use the package as usual:

```r
con$list_mail_folders()
con$select_folder("INBOX")
con$search_string(expr = "sales", where = "SUBJECT")
```

See the *"A reproducible IMAP sandbox with Docker"* vignette for a guided tour.

## Tear down

```sh
docker rm -f mrpostman-sandbox
```

Removing the container discards the mailbox; a fresh `docker run` +
`populate_sandbox()` recreates it identically.
