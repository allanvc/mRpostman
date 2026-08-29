# Parse an IMAP ENVELOPE into a one-row data frame

Reads the `ENVELOPE (...)` structure of a `FETCH` response (RFC 3501,
section 7.4.2), as returned by `ImapCon$fetch_metadata(attribute =
"ENVELOPE")`, into analysis-ready columns. Address lists are formatted
as `Name <mailbox@host>` and joined with commas; RFC 2047 encoded words
in names and subjects are decoded.

## Usage

``` r
parse_envelope(x)
```

## Arguments

  - x:
    
    A `character` string with a `FETCH` response (or the part of it
    starting at `ENVELOPE`).

## Value

A one-row `data.frame` with columns `date`, `subject`, `from`, `sender`,
`reply_to`, `to`, `cc`, `bcc`, `in_reply_to`, and `message_id` (`NA`
where the message has no value).

## Examples

``` r
x <- paste0('ENVELOPE ("Mon, 7 May 2001 08:41:00 -0700" "A resume" ',
            '(("Vince Kaminski" NIL "vince.kaminski" "enron.com")) NIL NIL ',
            '((NIL NIL "stephen.stock" "enron.com")) NIL NIL NIL "<id@x>")')
parse_envelope(x)
#>                             date  subject
#> 1 Mon, 7 May 2001 08:41:00 -0700 A resume
#>                                        from sender reply_to
#> 1 Vince Kaminski <vince.kaminski@enron.com>   <NA>     <NA>
#>                        to   cc  bcc in_reply_to message_id
#> 1 stephen.stock@enron.com <NA> <NA>        <NA>     <id@x>
```
