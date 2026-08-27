**mRpostman - Update to v2.1.0**

This is a major release. The package now has a small compiled component
(`src/imap_socket.c`, linking libcurl, already a system requirement) that
provides a raw TLS socket via libcurl's `CONNECT_ONLY` mode, used for `IDLE`
(RFC 2177), `MULTIAPPEND` (RFC 3502), `NOTIFY` (RFC 5465), `BINARY` (RFC 3516),
`CATENATE` (RFC 4469), and `COMPRESS=DEFLATE` (RFC 4978, which also links zlib). It also completes the IMAP4rev1 client command set
(`CHECK`) and adds the `UIDPLUS`, `LIST-STATUS`, `LIST-EXTENDED`, `ACL`,
`SETQUOTA`, `ENABLE`, `SEARCHRES`, `ESORT`, `PREVIEW`, `SAVEDATE`,
`STATUS=SIZE`, `CONDSTORE`, `QRESYNC`, and `METADATA`, all capability-checked; parses
`ENVELOPE` and `BODYSTRUCTURE` into data frames; and handles non-ASCII
mailbox names (modified UTF-7) and attachment extraction by MIME part. See
NEWS.md. No new dependencies.

## Test environments
* local Ubuntu 22.04, R 4.4.1
* Github Actions: macOS (release), Windows (release), Ubuntu (devel, release, oldrel-1)
* win-builder: devel

## R CMD check results
0 ERRORs, 0 WARNINGs, 0 NOTEs
