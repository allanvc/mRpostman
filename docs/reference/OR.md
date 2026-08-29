# Relational-operator-function to construct a custom search statement

Relational-operator-function to construct a custom search statement

## Usage

``` r
OR(..., negate = FALSE)
```

## Arguments

  - ...:
    
    a combination of criteria constructor functions with its arguments.

  - negate:
    
    If `TRUE`, negates the search and seeks for "NOT search\_criterion".
    Default is `FALSE`.

## Value

A search string to be used as a `request` parameter in
`ImapCon$search()` function.

## See also

Other custom search: `AND()`, `ImapCon`, `before()`, `filter_stored()`,
`flag()`, `fuzzy()`, `larger_than()`, `modseq()`, `older_than()`,
`on()`, `saved_before()`, `sent_before()`, `sent_on()`, `sent_since()`,
`since()`, `smaller_than()`, `string()`, `verbatim()`, `younger_than()`

## Examples

``` r
if (FALSE) { # \dontrun{
# select folder & search
con$select_folder(folder = "INBOX")
# search for messages SINCE "30-Aug-2019" OR SMALLER than 512KB.
res <- con$search(request = OR(sent_since(date_char = "30-Aug-2019"),
                                smaller_than(size = 512000)))
} # }
```
