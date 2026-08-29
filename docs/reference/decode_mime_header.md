# Decode RFC 2047 quoted-printable and base64 MIME headers and strings

Decode RFC 2047 quoted-printable and base64 MIME headers and strings

## Usage

``` r
decode_mime_header(string)
```

## Arguments

  - string:
    
    A `character` vector containing a string to be decoded.

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

## Examples

``` r
if (FALSE) { # \dontrun{
# The examples below run smoothly on any computer. The 'dontrun' flag is just to skip CRAN checks.

# Simple quoted-printable string - Portuguese example
qp_encoded <- "Minist=E9rio_da_Educa=E7=E3o"
decode_mime_header(string = qp_encoded)

# Simple quoted-printable string - French example
qp_encoded <- "sur la route =C3=A0 suivre les voil=C3=A0 bient=C3=B4t qui te d=C3=A9gradent"
decode_mime_header(string = qp_encoded)

# Simple quoted-printable string - Norwegian example
qp_encoded <- "p=C3=A5 veien for =C3=A5 f=C3=B8lge, snart vil de forringe deg"
decode_mime_header(string = qp_encoded)

# Simple quoted-printable string - Turkish example
qp_encoded <- "yak=C4=B1nda seni k=C3=BC=C3=A7=C3=BCk d=C3=BC=C5=9F=C3=BCrecekler"
decode_mime_header(string = qp_encoded)

# RFC 2047 quoted-printable header - Portuguese example
qp_encoded <- "=?iso-8859-1?Q?DIDEC_Capacita=E7=E3o?="
decode_mime_header(string = qp_encoded)

# RFC 2047 quoted-printable - German example
qp_encoded <- "=?UTF-8?Q?stern=2Ede_-_t=C3=A4glich?="
decode_mime_header(string = qp_encoded)

# RFC 2047 base64 - Portuguese example
b64_encoded <- "=?utf-8?B?Sk9BTkEgRlVTQ08gTE9CTyBubyBUZWFtcw==?="
decode_mime_header(string = b64_encoded)
} # }
```
