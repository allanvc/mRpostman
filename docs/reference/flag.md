# Criterion constructor function to be combined in a custom search statement

Criterion constructor function to be combined in a custom search
statement

## Usage

``` r
flag(name, negate = FALSE)
```

## Arguments

  - name:
    
    A string containing one or more flags to search for. Use
    [`ImapCon$list_flags()`](#method-list_flags) to list the flags in a
    selected mail folder.

  - negate:
    
    If `TRUE`, negates the search and seeks for "NOT SEARCH CRITERIA".
    Default is `FALSE`.

## Value

A search criterion of class `imap_search`, to be combined into a search
statement (see `Ops.imap_search`).

## See also

Other custom search: `AND()`, `ImapCon`, `OR()`, `before()`,
`filter_stored()`, `fuzzy()`, `larger_than()`, `modseq()`,
`older_than()`, `on()`, `saved_before()`, `sent_before()`, `sent_on()`,
`sent_since()`, `since()`, `smaller_than()`, `string()`, `verbatim()`,
`younger_than()`

## Examples

``` r
if (FALSE) { # \dontrun{
# select folder & search
con$select_folder(folder = "INBOX")
# search for messages with Flag "UNSEEN" AND NOT Smaller Than  512KB.
res <- con$search(request = AND(flag("UNSEEN"),
                                smaller_than(size = 512000, negate = TRUE)))
} # }
```
