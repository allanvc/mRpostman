# Encode and decode mailbox names in IMAP modified UTF-7

IMAP mailbox names are transmitted in the modified UTF-7 encoding of RFC
3501 (section 5.1.3): non-ASCII runs are written as `&<base64>-`, where
the base64 alphabet uses `,` instead of `/` and carries no padding, and
a literal `&` is written `&-`. `mRpostman` applies the encoding to every
folder name it sends and decodes the names it receives, so folders can
be referred to by their real (UTF-8) names; these helpers are exported
for users who handle raw names.

## Usage

``` r
imap_utf7_encode(x)

imap_utf7_decode(x)
```

## Arguments

  - x:
    
    A `character` vector of mailbox names.

## Value

A `character` vector of the same length.

## Examples

``` r
imap_utf7_encode("École")   # "&AMk-cole"
#> [1] "&AMk-cole"
imap_utf7_decode("&AMk-cole")    # "École"
#> [1] "École"
imap_utf7_encode("Q&A")          # "Q&-A"
#> [1] "Q&-A"
```
