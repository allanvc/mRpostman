# Message Metadata Options

List Metadata fields used in messages.

## Usage

``` r
metadata_options()
```

## Value

A `vector` containing message metadata fields.

## Note

This function lists the message attributes that
[`ImapCon$fetch_metadata()`](#method-fetch_metadata) accepts. The last
three (`PREVIEW`, `SAVEDATE`, `MODSEQ`) are capability-gated extensions,
only sent when the server advertises them.

## References

Crispin, M., "Internet Message Access Protocol - Version 4rev1", RFC
2060, [doi:10.17487/RFC2060](https://doi.org/10.17487/RFC2060) ,
December 1996, <https://www.rfc-editor.org/info/rfc2060>.

## Examples

``` r
if (FALSE) { # \dontrun{

library(mRpostman)
metadata_options()

} # }
```
