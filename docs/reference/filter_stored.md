# Criterion constructor referencing a filter stored on the server

Builds a `FILTER <name>` search criterion (RFC 5466), which stands for
the search criteria saved on the server under that name through the
ManageSieve protocol. Requires the server `FILTERS` capability, which is
checked when the search is executed. Experimental: none of the reference
servers used to validate this package advertises `FILTERS`, so the
criterion follows the RFC grammar but has not been exercised against a
live server.

## Usage

``` r
filter_stored(name)
```

## Arguments

  - name:
    
    A `character` string with the name of the filter stored on the
    server.

## Value

A search criterion of class `imap_search`, to be combined into a search
statement (see `Ops.imap_search`).

## See also

Other custom search: `AND()`, `ImapCon`, `OR()`, `before()`, `flag()`,
`fuzzy()`, `larger_than()`, `modseq()`, `older_than()`, `on()`,
`saved_before()`, `sent_before()`, `sent_on()`, `sent_since()`,
`since()`, `smaller_than()`, `string()`, `verbatim()`, `younger_than()`

## Examples

``` r
if (FALSE) { # \dontrun{
con$select_folder(folder = "INBOX")
res <- con$search(request = AND(filter_stored("on-the-road"),
                                string(expr = "boss@example.com", where = "FROM")))
} # }
```
