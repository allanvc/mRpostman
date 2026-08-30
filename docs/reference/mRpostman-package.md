# An IMAP client for R

mRpostman is a session-based IMAP client that implements the full
command sets of the IMAP4rev2 (RFC 9051) and IMAP4rev1 (RFC 3501)
protocols, along with the optional extensions registered with IANA,
allowing virtually all e-mail operations to be performed from within R,
paving the way for e-mail data analysis.

## Options

  - `mRpostman.raw_debug`:
    
    If `TRUE`, prints the raw dialogue of the second (event) connection
    used by `idle()`, `append_msgs()`, and the other raw-socket
    commands, in the same spirit as `verbose = TRUE` for the main
    connection. Default is `FALSE`.

## Author

Author & Mantainer: Allan Quadros <allanvcq@gmail.com>

## References

Crispin, M. (2003), *INTERNET MESSAGE ACCESS PROTOCOL - VERSION 4rev1*,
RFC 3501, March 2003, <https://www.rfc-editor.org/rfc/rfc3501>.

Heinlein, P. and Hartleben, P. (2008). *The Book of IMAP: Building a
Mail Server with Courier and Cyrus*. No Starch Press. ISBN
978-1-59327-177-0.

Ooms, J. (2020). *curl: A Modern and Flexible Web Client for R*. R
package version 4.3, <https://CRAN.R-project.org/package=curl>.

Stenberg, D. *Libcurl - The Multiprotocol File Transfer Library*,
<https://curl.se/libcurl/>.

## See also

Useful links:

  - `mRpostman official website`: <https://allanvc.github.io/mRpostman/>
