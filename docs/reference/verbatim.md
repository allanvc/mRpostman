# Criterion constructor function for a verbatim IMAP search fragment

Wraps any raw IMAP `SEARCH` fragment as a search criterion, so that
protocol keys without a dedicated field in
[`ImapCon$query()`](#method-query) - vendor extensions such as Gmail's
`X-GM-RAW`, sequence sets such as `UID 100:200`, or `FUZZY` matches (RFC
6203) - can be combined with the rest of the search language or with the
other criterion constructors. The fragment is passed to the server
exactly as provided (parenthesized if it is not already), and its keys
are not validated locally: an unknown key results in a server-side `BAD`
response.

## Usage

``` r
verbatim(request)
```

## Arguments

  - request:
    
    A character string with the raw search fragment.

## Value

A search criterion of class `imap_search`, to be combined into a search
statement (see `Ops.imap_search`).

## See also

Other custom search: `AND()`, `ImapCon`, `OR()`, `before()`,
`filter_stored()`, `flag()`, `fuzzy()`, `larger_than()`, `modseq()`,
`older_than()`, `on()`, `saved_before()`, `sent_before()`, `sent_on()`,
`sent_since()`, `since()`, `smaller_than()`, `string()`,
`younger_than()`

## Examples

``` r
if (FALSE) { # \dontrun{
# a vendor extension combined with a regular field
res <- con$query(verbatim('X-GM-RAW "has:attachment smaller:25M"') &
                   flag != "SEEN")
# a UID range in a classic custom search
res <- con$search(request = AND(verbatim("UID 100:200"),
                                string(expr = "@gmail.com", where = "FROM")),
                  use_uid = TRUE)
} # }
```
