# Criterion constructor function to be combined in a custom search statement

Criterion constructor function to be combined in a custom search
statement

## Usage

``` r
older_than(seconds, negate = FALSE)
```

## Arguments

  - seconds:
    
    An integer specifying the number of seconds to be used as the search
    criterion.

  - negate:
    
    If `TRUE`, negates the search and seeks for "NOT SEARCH CRITERIA".
    Default is `FALSE`.

## Value

A search criterion of class `imap_search`, to be combined into a search
statement (see `Ops.imap_search`).

## Note

To be able to use this functionality, the server must support the
`WITHIN` capability.

## See also

Other custom search: `AND()`, `ImapCon`, `OR()`, `before()`,
`filter_stored()`, `flag()`, `fuzzy()`, `larger_than()`, `modseq()`,
`on()`, `saved_before()`, `sent_before()`, `sent_on()`, `sent_since()`,
`since()`, `smaller_than()`, `string()`, `verbatim()`, `younger_than()`

## Examples

``` r
if (FALSE) { # \dontrun{
# select folder & search
con$select_folder(folder = "INBOX")
# search for messages containing the string "XYZ@k-state.edu" in the
#   "FROM" field AND those that are OLDER than 3600 seconds (1 hour).
res <- con$search(request = AND(string(expr = "XYZ@k-state.edu",
                                      where = "FROM"),
                               older_than(seconds = 3600)))
} # }
```
