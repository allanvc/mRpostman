# Ingest a Local Maildir into an IMAP Folder

Uploads every message file found under a local directory to a folder on
the connected IMAP server, using the package's own `APPEND`
implementation (`ImapCon$append_msg()`). Message files are read as raw
bytes and appended verbatim, so any RFC 5322-conformant maildir-style
layout (one message per file) can be ingested – mail server backups,
exported archives, or public corpora such as the Enron corpus (see
`enron_sandbox`).

## Usage

``` r
ingest_maildir(
  con,
  path = NULL,
  folder = basename(path),
  create = TRUE,
  recursive = TRUE,
  files = NULL,
  mute = FALSE,
  retries = 1
)
```

## Arguments

  - con:
    
    An `ImapCon` object, as returned by `configure_imap`.

  - path:
    
    A string with the path to a local directory containing message
    files.

  - folder:
    
    A string with the name of the destination folder on the server.
    Default is `basename(path)`.

  - create:
    
    A `logical`. If `TRUE` (default), the destination folder is created
    with `CREATE` when it does not exist yet.

  - recursive:
    
    A `logical`. If `TRUE` (default), message files are searched
    recursively under `path`.

  - files:
    
    An optional character vector of message file paths to ingest,
    overriding the directory listing. When provided, `path` and
    `recursive` are ignored for file discovery (a subset selected by the
    caller – see `enron_sandbox` – is uploaded as given). Default is
    `NULL`.

  - mute:
    
    A `logical`. If `FALSE`, prints progress every 500 messages and a
    final summary. Default is `FALSE`.

  - retries:
    
    Number of attempts for each `APPEND`. Default is `1`.

## Value

Invisibly, a `data.frame` with one row per message file: `path`, `size`,
`appended` (a `logical` indicating whether the upload succeeded), and
`uid` (the UID the server assigned to the message, when it advertises
`UIDPLUS`; `NA` otherwise).

## Details

`APPEND` stores every message with the current time as its internal date
(see `populate_sandbox` for the implications), so header-based `SENT*`
searches are the appropriate way to query ingested corpora by date.
Messages are stored without flags on libcurl \>= 8.13 (earlier versions
hardcode `\Seen` in `APPEND`); use `ImapCon$add_flags()` to mark them as
read if desired. Files that fail to upload (e.g., malformed messages
rejected by the server) are skipped with a warning rather than aborting
the ingestion.

## See also

Other sandbox: `enron_sandbox()`, `populate_sandbox()`,
`sandbox_corpus()`

## Examples

``` r
if (FALSE) { # \dontrun{
con <- configure_imap(url = "imap://localhost:1430",
                      username = "testuser", password = "sandbox",
                      use_ssl = FALSE)
# upload a locally stored maildir folder to the server
manifest <- ingest_maildir(con, path = "~/backup/maildir/archive2020",
                           folder = "archive2020")
table(manifest$appended)
} # }
```
