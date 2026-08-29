# Generate a Deterministic Synthetic Mail Corpus

Generates a reproducible corpus of synthetic RFC 822 messages to be
stored in a mailbox with `populate_sandbox`, so that the package's
searching, fetching, and decoding features can be demonstrated and
tested without a real mail account – typically against the local Docker
IMAP sandbox shipped in `system.file("docker", package = "mRpostman")`.

## Usage

``` r
sandbox_corpus(n = 200, seed = 3501)
```

## Arguments

  - n:
    
    Number of messages to generate. Default is `200`.

  - seed:
    
    An integer used as the RNG seed. The same `n` and `seed` always
    produce byte-identical messages. Default is `3501`, after RFC 3501.
    The caller's RNG state is preserved.

## Value

A `list` with two components: `messages`, a list of `n` character
vectors (the lines of each RFC 822 message); and `info`, a `data.frame`
with one row per message describing the features embedded in it (`from`,
`subject`, `date`, `is_utf8_body`, `is_large`, `has_attachment`,
`attachment_type` (`"csv"`, `"png"`, `"pdf"`, or `NA`), `is_reply`, and
the flags – `seen`, `flagged` – that `populate_sandbox` will set after
appending).

## Details

Message features are spread so that each capability of the package has
matching messages to act upon: `Date:` headers spread over 2020
(`SENTBEFORE`/`SENTSINCE`/`SENTON` searches); a subset of large bodies
(`LARGER`/`SMALLER` searches); accented subjects in MIME encoded-words
and quoted-printable bodies (header and body decoding); CSV, PNG, and
one-page PDF attachments – all generated deterministically in base R –
some with repeated filenames (attachment listing, fetching, binary
base64 decoding, and filename deduplication); and reply chains via
`In-Reply-To`/`References` (`SORT` and `THREAD`).

## See also

Other sandbox: `enron_sandbox()`, `ingest_maildir()`,
`populate_sandbox()`

## Examples

``` r
corpus <- sandbox_corpus(n = 5)
# the first synthetic message
cat(corpus$messages[[1]], sep = "\n")
#> From: Grace Holloway <grace.holloway@example.net>
#> To: Test User <testuser@sandbox.local>
#> Date: Sun, 26 Apr 2020 19:34:07 +0000
#> Message-ID: <msg0001@sandbox.local>
#> Subject: Invoice attached
#> MIME-Version: 1.0
#> Content-Type: text/plain; charset=utf-8
#> Content-Transfer-Encoding: quoted-printable
#> 
#> This supersedes the previous version sent last week.
#> 
#> Let me know if you need any further details before the meeting.
#> 
# features embedded in each message
corpus$info
#>   id                       from                       subject
#> 1  1 grace.holloway@example.net              Invoice attached
#> 2  2   alice.fontes@example.com           Relatório de vendas
#> 3  3   carla.mendes@example.com       Re: Travel arrangements
#> 4  4 grace.holloway@example.net Re: Server maintenance window
#> 5  5 grace.holloway@example.net     Server maintenance window
#>                              date is_utf8_body is_large has_attachment
#> 1 Sun, 26 Apr 2020 19:34:07 +0000        FALSE    FALSE          FALSE
#> 2 Thu, 04 Jun 2020 13:17:14 +0000         TRUE    FALSE          FALSE
#> 3 Sun, 28 Jun 2020 15:31:21 +0000        FALSE    FALSE           TRUE
#> 4 Wed, 16 Sep 2020 19:57:28 +0000        FALSE     TRUE          FALSE
#> 5 Fri, 25 Sep 2020 18:45:35 +0000        FALSE    FALSE           TRUE
#>   attachment_type is_reply  seen flagged
#> 1            <NA>    FALSE  TRUE   FALSE
#> 2            <NA>    FALSE  TRUE   FALSE
#> 3             csv     TRUE FALSE   FALSE
#> 4            <NA>     TRUE  TRUE    TRUE
#> 5             pdf    FALSE  TRUE    TRUE
```
