# Criterion constructor function to be combined in a custom search statement

Criterion constructor function to be combined in a custom search
statement

## Usage

``` r
string(expr, where, negate = FALSE)
```

## Arguments

  - expr:
    
    A character string specifying the word or expression to search for
    in messages.

  - where:
    
    A mandatory character string specifying in which message's Section
    or Header Field to search for the provided string.

  - negate:
    
    If `TRUE`, negates the search and seeks for "NOT SEARCH CRITERIA".
    Default is `FALSE`.

## Value

A search criterion of class `imap_search`, to be combined into a search
statement (see `Ops.imap_search`).

## See also

Other custom search: `AND()`, `ImapCon`, `OR()`, `before()`,
`filter_stored()`, `flag()`, `fuzzy()`, `larger_than()`, `modseq()`,
`older_than()`, `on()`, `saved_before()`, `sent_before()`, `sent_on()`,
`sent_since()`, `since()`, `smaller_than()`, `verbatim()`,
`younger_than()`

## Examples

``` r
if (FALSE) { # \dontrun{
# select folder & search
con$select_folder(folder = "INBOX")
# search for messages containing the string "XYZ@k-state.edu" in the
#   "FROM" AND the string "@gmail.com" in the "CC" field.
res <- con$search(request = AND(string(expr = "XYZ@k-state.edu",
                                      where = "FROM"),
                               string(expr = "@gmail.com",
                                      where = "CC")))
} # }
```
