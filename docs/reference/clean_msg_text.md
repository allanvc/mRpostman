# Extract text from MIME level

Extract text from MIME level

## Usage

``` r
clean_msg_text(msg_list)
```

## Arguments

  - msg\_list:
    
    A `list` with the MIME level 1 of the body or text content of the
    messages fetched with [`ImapCon$fetch_body()`](#method-fetch_body)
    or [`ImapCon$fetch_text()`](#method-fetch_text).

## Value

A `list` containing the decoded messages if applicable.

## References

Moore, K. (1996), MIME (Multipurpose Internet Mail Extensions) Part
Three: Message Header Extensions for Non-ASCII Text, RFC 2047, November
1996, https://tools.ietf.org/html/rfc2047.

Freed, N., Borenstein, N. (1996), Multipurpose Internet Mail Extensions
(MIME) Part One: Format of Internet Message Bodies, RFC 2045, November
1996, https://tools.ietf.org/html/rfc2045.

Internal parts of this object, regarding the quoted printable type, were
borrowed from https://github.com/hrbrmstr/hrbrmisc/blob/master/R/qp.r
with slight modifications.

## Examples

``` r
if (FALSE) { # \dontrun{
ids <- con$search_since(date_char = "01-Apr-2020", use_uid = TRUE)

fetch_res <- ids %>%
  con$fetch_body(use_uid = TRUE, mime_level = 1L)

clean_text_list <- clean_msg_text(msg_list = fetch_res)
} # }
```
