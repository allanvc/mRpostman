# An IMAP URL naming a message, or a part of it, for CATENATE (RFC 5092)

Builds the relative URL that `ImapCon$append_catenate()` uses to refer
to a message stored on the server: `/<folder>;UID=<uid>` with an
optional `/;SECTION=<section>` (e.g. `"HEADER"`, `"TEXT"`, or a MIME
part number such as `"2"`).

## Usage

``` r
imap_url(folder, uid, section = NULL)
```

## Arguments

  - folder:
    
    A `character` string with the folder name.

  - uid:
    
    The message UID.

  - section:
    
    `NULL` (the whole message) or a section specifier.

## Value

An object of class `imap_url`.

## Examples

``` r
imap_url("INBOX", uid = 12)
#> [1] "/INBOX/;UID=12"
#> attr(,"class")
#> [1] "imap_url"
imap_url("INBOX", uid = 12, section = "HEADER")
#> [1] "/INBOX/;UID=12/;SECTION=HEADER"
#> attr(,"class")
#> [1] "imap_url"
```
