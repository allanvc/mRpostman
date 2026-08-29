# Criterion constructor function to be combined in a custom search statement

Criterion constructor function to be combined in a custom search
statement

## Usage

``` r
on(date_char, negate = FALSE)
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

Other custom search: `AND()`, `ImapCon`, `OR()`, `before()`,
`filter_stored()`, `flag()`, `fuzzy()`, `larger_than()`, `modseq()`,
`older_than()`, `saved_before()`, `sent_before()`, `sent_on()`,
`sent_since()`, `since()`, `smaller_than()`, `string()`, `verbatim()`,
`younger_than()`

## Examples

``` r
if (FALSE) { # \dontrun{
# select folder & search
con$select_folder(folder = "INBOX")
# search for messages SINCE "17-Apr-2019" AND SMALLER than 512KB.
res <- con$search(request = OR(on(date_char = "30-Jun-2019"),
                               on(date_char = "22-Mar-2018")))
# search for messages received ON "30-Jun-2019" OR ON "22-Mar-2018".

} # }
```
