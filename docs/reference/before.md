# Criterion constructor function to be combined in a custom search statement

Criterion constructor function to be combined in a custom search
statement

## Usage

``` r
before(date_char, negate = FALSE)
```

## Arguments

  - date\_char:
    
    A `character string` with format "DD-Mon-YYYY", e.g. "01-Apr-2019".
    We opt not to use `Date` or `POSIX*` like objects, since IMAP
    servers use this unusual date format.

  - negate:
    
    If `TRUE`, negates the search and seeks for "NOT SEARCH CRITERIA".
    Default is `FALSE`.

## Value

A search string to be used as a `request` parameter in
`ImapCon$search()` function.

## See also

Other custom search: `AND()`, `ImapCon`, `OR()`, `filter_stored()`,
`flag()`, `fuzzy()`, `larger_than()`, `modseq()`, `older_than()`,
`on()`, `saved_before()`, `sent_before()`, `sent_on()`, `sent_since()`,
`since()`, `smaller_than()`, `string()`, `verbatim()`, `younger_than()`

## Examples

``` r
if (FALSE) { # \dontrun{
# select folder & search
con$select_folder(folder = "INBOX")
# search for messages BEFORE "17-Apr-2019" AND NOT SMALLER than 512KB.
res <- con$search(request = AND(before(date_char = "17-Apr-2019"),
                                smaller_than(size = 512000, negate = TRUE)))
} # }
```
