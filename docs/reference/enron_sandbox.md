# Populate a Mailbox with a Subset of the Enron Corpus

Downloads the public Enron e-mail corpus (CMU, May 7 2015 release; \~423
MB, one time, cached locally), selects a subset by custodian, folder,
and date, and ingests it into the connected IMAP server through
`ingest_maildir` – i.e., through the package's own `APPEND`
implementation. One server folder is created per custodian. It is meant
to be run against the disposable local Docker IMAP server shipped in
`system.file("docker", package = "mRpostman")`, providing real data for
demonstrations and teaching on top of the synthetic corpus of
`sandbox_corpus`.

## Usage

``` r
enron_sandbox(
  con,
  custodians = c("kaminski-v", "lay-k", "skilling-j"),
  folder_pattern = "^_?sent(_items|_mail)?$",
  sent_since = NULL,
  sent_before = NULL,
  max_msgs = Inf,
  cache_dir = tools::R_user_dir("mRpostman", "cache"),
  url = "https://www.cs.cmu.edu/~enron/enron_mail_20150507.tar.gz",
  ask = TRUE,
  mute = FALSE
)
```

## Arguments

  - con:
    
    An `ImapCon` object, as returned by `configure_imap`.

  - custodians:
    
    A character vector of custodian directory names, e.g.
    `c("kaminski-v", "lay-k", "skilling-j")` (the default). The 150
    available names are the directories of the corpus' `maildir/` root.

  - folder\_pattern:
    
    A regular expression matched against the custodians' folder names.
    The default, `"^_?sent(_items|_mail)?$"`, selects the sent-mail
    folder variants. Use `".*"` for all folders of the selected
    custodians.

  - sent\_since, sent\_before:
    
    Optional `Date` (or `"YYYY-MM-DD"` string) limits applied to each
    message's `Date:` header before ingestion. `NULL` (default) applies
    no limit.

  - max\_msgs:
    
    Maximum number of messages per custodian, applied after the other
    filters. Default is `Inf`.

  - cache\_dir:
    
    Directory where the corpus tarball and its extraction are cached
    across sessions. Default is `tools::R_user_dir("mRpostman",
    "cache")`.

  - url:
    
    URL of the corpus tarball. Default is the CMU distribution point.

  - ask:
    
    A `logical`. If `TRUE` (default), asks for confirmation before the
    one-time \~423 MB download (\~1.4 GB extracted). Set to `FALSE` for
    non-interactive use.

  - mute:
    
    A `logical`. Passed to `ingest_maildir`. Default is `FALSE`.

## Value

Invisibly, a `data.frame` with one row per ingested message:
`custodian`, `path` (relative to the corpus root), `size`, `date`
(parsed from the header; `NA` when unparseable), and `appended`.

## Details

The corpus was released into the public domain by the U.S. Federal
Energy Regulatory Commission and is curated at Carnegie Mellon
University (<https://www.cs.cmu.edu/~enron/>); it contains about 517,000
messages from 150 custodians in maildir layout, without attachments.
Nothing is downloaded when the cache is already in place. If the
download fails (offline use, moved URL), the function fails gracefully
with an informative message and leaves the cache untouched. Date
filtering parses each candidate file's `Date:` header
locale-independently; messages without a parseable date are kept unless
both limits are set.

## See also

Other sandbox: `ingest_maildir()`, `populate_sandbox()`,
`sandbox_corpus()`

## Examples

``` r
if (FALSE) { # \dontrun{
con <- configure_imap(url = "imap://localhost:1430",
                      username = "enron", password = "sandbox",
                      use_ssl = FALSE)
# sent mail of three central custodians, 2000-2002
manifest <- enron_sandbox(con,
                          custodians = c("kaminski-v", "lay-k",
                                         "skilling-j"),
                          sent_since  = "2000-01-01",
                          sent_before = "2002-07-01")
table(manifest$custodian, manifest$appended)
} # }
```
