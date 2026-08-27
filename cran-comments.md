**mRpostman - Update to v2.2.0**

This release completes the package's coverage of the IANA IMAP capability
registry. Exercised against live servers: `SORT=DISPLAY` (RFC 5957),
`THREAD=REFS`, paged searching (`PARTIAL`, RFC 9394 / `CONTEXT=SEARCH`, RFC
5267), non-synchronizing literals (`LITERAL+`/`LITERAL-`, RFC 7888), and the
`APPENDLIMIT` guard (RFC 7889). The remaining registered capabilities
(`REPLACE`, `OBJECTID`, `UIDBATCHES`, `MULTISEARCH`, `UNAUTHENTICATE`,
`LANGUAGE`/`I18NLEVEL=2`, `URLAUTH`, `CONVERT`, `ANNOTATE-EXPERIMENT-1`,
`CONTEXT=SORT`, `SEARCH=FUZZY`, `FILTERS`) are implemented from the RFC
grammars and documented as experimental, since no widely deployed server
advertises them; all are capability-checked before any command is issued.
The DESCRIPTION now presents the package as an IMAP4rev2 (RFC 9051) and
IMAP4rev1 (RFC 3501) client. See NEWS.md. No new dependencies.

## Test environments
* local Ubuntu 22.04, R 4.4.1
* Github Actions: macOS (release), Windows (release), Ubuntu (devel, release, oldrel-1)
* win-builder: devel

## R CMD check results
0 ERRORs, 0 WARNINGs, 0 NOTEs
