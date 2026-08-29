# Criterion constructor function for the CONDSTORE extension (RFC 7162)

Builds the `MODSEQ` search key, which matches the messages whose
modification sequence is equal to or greater than `value`, i.e. the
messages that were added or whose flags changed since the folder had
that `HIGHESTMODSEQ` (obtained with `ImapCon$status(items =
"HIGHESTMODSEQ")`). It is to be combined with `AND` and `OR` and passed
to `ImapCon$search()`. The server must advertise the `CONDSTORE`
capability.

## Usage

``` r
modseq(value, negate = FALSE)
```

## Arguments

  - value:
    
    A single non-negative number, the modification sequence to compare
    with.

  - negate:
    
    If `TRUE`, negates the search and seeks for "NOT SEARCH CRITERIA".
    Default is `FALSE`.

## Value

A `character` string with the search criterion.

## See also

Other custom search: `AND()`, `ImapCon`, `OR()`, `before()`,
`filter_stored()`, `flag()`, `fuzzy()`, `larger_than()`, `older_than()`,
`on()`, `saved_before()`, `sent_before()`, `sent_on()`, `sent_since()`,
`since()`, `smaller_than()`, `string()`, `verbatim()`, `younger_than()`

## Examples

``` r
if (FALSE) { # \dontrun{
con$select_folder(folder = "INBOX")
last <- con$status(items = "HIGHESTMODSEQ")[["HIGHESTMODSEQ"]]
# ... later, in another run:
changed <- con$search(request = modseq(last + 1))
} # }
```
