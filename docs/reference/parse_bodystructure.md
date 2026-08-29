# Parse an IMAP BODYSTRUCTURE into a data frame of MIME parts

Reads the `BODYSTRUCTURE (...)` structure of a `FETCH` response (RFC
3501, section 7.4.2), as returned by `ImapCon$fetch_metadata(attribute =
"BODYSTRUCTURE")`, into one row per MIME part, numbered as in `FETCH
BODY[<part>]`.

## Usage

``` r
parse_bodystructure(x)
```

## Arguments

  - x:
    
    A `character` string with a `FETCH` response (or the part of it
    starting at `BODYSTRUCTURE` or `BODY`).

## Value

A `data.frame` with columns `part` (the section number, e.g. `"1"`,
`"2.1"`; multipart containers are listed with `part = NA`), `type`,
`subtype`, `charset`, `filename`, `encoding`, `size` (bytes, `NA` for
containers), `disposition`, and `is_attachment`.

## Examples

``` r
x <- paste0('BODYSTRUCTURE (("text" "plain" ("charset" "utf-8") NIL NIL ',
            '"quoted-printable" 120 3 NIL NIL NIL NIL)',
            '("application" "pdf" ("name" "report.pdf") NIL NIL "base64" ',
            '4096 NIL ("attachment" ("filename" "report.pdf")) NIL NIL) ',
            '"mixed" ("boundary" "xyz") NIL NIL NIL)')
parse_bodystructure(x)
#>   part        type subtype charset   filename         encoding size disposition
#> 1 <NA>   multipart   mixed    <NA>       <NA>             <NA>   NA        <NA>
#> 2    1        text   plain   utf-8       <NA> quoted-printable  120        <NA>
#> 3    2 application     pdf    <NA> report.pdf           base64 4096  attachment
#>   is_attachment
#> 1         FALSE
#> 2         FALSE
#> 3          TRUE
```
