# Criterion constructor function to be combined in a custom search statement

Criterion constructor function to be combined in a custom search
statement

## Usage

``` r
sent_since(date_char, negate = FALSE)
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
`older_than()`, `on()`, `saved_before()`, `sent_before()`, `sent_on()`,
`since()`, `smaller_than()`, `string()`, `verbatim()`, `younger_than()`

## Examples

``` r
if (FALSE) { # \dontrun{
# select folder & search
con$select_folder(folder = "INBOX")
# search for messages SENT SINCE "22-Mar-2020" OR containing the STRING
#  "congratulations" in the subject.
res <- con$search(request = AND(sent_since(date_char = "22-Mar-2020"),
                                string(expr = "congratulations",
                                       where = "SUBJECT")))
} # }
```
