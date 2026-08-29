# Criterion constructor functions for the SAVEDATE extension (RFC 8514)

Build the `SAVEDBEFORE`, `SAVEDON`, and `SAVEDSINCE` search keys, which
compare the date a message was saved into the mailbox (the `SAVEDATE`
attribute) rather than its internal date or its `Date:` header. They are
to be combined with `AND` and `OR` and passed to `ImapCon$search()`. The
server must advertise the `SAVEDATE` capability.

## Usage

``` r
saved_before(date_char, negate = FALSE)

saved_since(date_char, negate = FALSE)

saved_on(date_char, negate = FALSE)
```

## Arguments

  - date\_char:
    
    A `character` string with a date in the IMAP format `DD-Mon-YYYY`,
    e.g. `"17-Apr-2019"`.

  - negate:
    
    If `TRUE`, negates the search and seeks for "NOT SEARCH CRITERIA".
    Default is `FALSE`.

## Value

A `character` string with the search criterion.

## See also

Other custom search: `AND()`, `ImapCon`, `OR()`, `before()`,
`filter_stored()`, `flag()`, `fuzzy()`, `larger_than()`, `modseq()`,
`older_than()`, `on()`, `sent_before()`, `sent_on()`, `sent_since()`,
`since()`, `smaller_than()`, `string()`, `verbatim()`, `younger_than()`

## Examples

``` r
if (FALSE) { # \dontrun{
con$select_folder(folder = "INBOX")
# messages saved into the folder during the last ingestion day
res <- con$search(request = saved_since(date_char = "27-Aug-2026"))
} # }
```
