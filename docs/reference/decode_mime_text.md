# Decode RFC 2047 quoted-printable and base64 MIME encoded text

Decode RFC 2047 quoted-printable and base64 MIME encoded text

## Usage

``` r
decode_mime_text(string, charset = NULL)
```

## Arguments

  - string:
    
    A `character` vector containing a string to be decoded.

  - charset:
    
    A `character` string with the charset declared in the MIME
    `Content-Type`. When supplied it is honored (via `apply_charset()`)
    instead of the legacy heuristic detection.

## Value

A decoded `character` vector if applicable.

## Note

The RFC 2047 (Moore, 1996) presents an encoded-word syntax to be used by
e-mail clients to display body text and header information in character
sets other than ASCII. According to the manual, non-ASCII content is
encoded as an ASCII text string as follows:
`=?<charset>?<encoding>?<encoded-text>?=`. The encoding can be of two
types: "B" for "BASE64", or "Q" for quoted- printable content (Freed and
Borenstein, 1996). Besides the standard RFC 2047 decoding, this function
also enables users to decode content that does not strictly follow the
`=?<charset>?<encoding>?<encoded-text>?=` RFC 2047 syntax, i.e. cases
where only the encoded text part is present, such as the
quoted-printable pattern in the string `"Estat=EDstica"` (Estatística,
which is the equivalent word, in Portuguese, for Statistics).

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
