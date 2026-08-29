# Combine search criteria with R's own operators

Criteria built with the constructor functions (`string()`, `flag()`,
`before()`, `sent_since()`, `larger_than()`, and their relatives) can be
combined with the native operators `&` (AND), `|` (OR), and `!` (NOT),
as an alternative to `AND` and `OR`. Precedence and grouping follow R's
own rules, so parentheses work as usual. See also `ImapCon$query()` for
the expression-based interface that does not require constructor calls
at all.

## Usage

``` r
# S3 method for class 'imap_search'
Ops(e1, e2)
```

## Arguments

  - e1, e2:
    
    Search criteria built by the constructor functions.

## Value

A search criterion string of class `"imap_search"`, to be passed to
`ImapCon$search()`.

## Examples

``` r
if (FALSE) { # \dontrun{
con$search(string("budget", where = "SUBJECT") &
             (sent_since(date_char = "01-Oct-2001") | !flag("SEEN")))
} # }
```
