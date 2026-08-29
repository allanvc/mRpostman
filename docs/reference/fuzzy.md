# Criterion modifier for fuzzy (approximate) matching in a custom search

Prefixes a search criterion with `FUZZY` (RFC 6203), asking the server
to match the criterion approximately (typically through its full-text
search index) instead of by exact substring. Requires the server
`SEARCH=FUZZY` capability, which is checked when the search is executed.
Experimental: none of the reference servers used to validate this
package advertises `SEARCH=FUZZY`, so the modifier follows the RFC
grammar but has not been exercised against a live server.

## Usage

``` r
fuzzy(criterion)
```

## Arguments

  - criterion:
    
    A search criterion string, usually built with `string` (e.g.
    `string(expr = "jump", where = "SUBJECT")`).

## Value

A search criterion of class `imap_search`, to be combined into a search
statement (see `Ops.imap_search`).

## See also

Other custom search: `AND()`, `ImapCon`, `OR()`, `before()`,
`filter_stored()`, `flag()`, `larger_than()`, `modseq()`,
`older_than()`, `on()`, `saved_before()`, `sent_before()`, `sent_on()`,
`sent_since()`, `since()`, `smaller_than()`, `string()`, `verbatim()`,
`younger_than()`

## Examples

``` r
if (FALSE) { # \dontrun{
con$select_folder(folder = "INBOX")
res <- con$search(request = fuzzy(string(expr = "jump", where = "SUBJECT")))
} # }
```
