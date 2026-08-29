# Criterion constructor function to be combined in a custom search statement

Criterion constructor function to be combined in a custom search
statement

## Usage

``` r
larger_than(size, negate = FALSE)
```

## Arguments

  - size:
    
    An integer specifying the number of bytes to be used as search
    criterion.

  - negate:
    
    If `TRUE`, negates the search and seeks for "NOT SEARCH CRITERIA".
    Default is `FALSE`.

## Value

A search criterion of class `imap_search`, to be combined into a search
statement (see `Ops.imap_search`).

## See also

Other custom search: `AND()`, `ImapCon`, `OR()`, `before()`,
`filter_stored()`, `flag()`, `fuzzy()`, `modseq()`, `older_than()`,
`on()`, `saved_before()`, `sent_before()`, `sent_on()`, `sent_since()`,
`since()`, `smaller_than()`, `string()`, `verbatim()`, `younger_than()`

## Examples

``` r
if (FALSE) { # \dontrun{
# select folder & search
con$select_folder(folder = "INBOX")
# search for messages containing the string "XYZ@k-state.edu" in the
#   "FROM" field OR those that are LARGER than 512KB.
res <- con$search(request = OR(string(expr = "XYZ@k-state.edu",
                                      where = "FROM"),
                               larger_than(size = 512000)))
} # }
```
