**mRpostman - Update to v1.5.1**

This is a feature release: it completes the IMAP4rev1 client command set
(`CHECK`) and adds the `UIDPLUS`, `LIST-STATUS`, `ACL`, `SETQUOTA`, `ENABLE`,
`SEARCHRES`, and `ESORT` extensions, all capability-checked (see NEWS.md).
No new dependencies.

## Test environments
* local Ubuntu 22.04, R 4.4.1
* Github Actions: macOS (release), Windows (release), Ubuntu (devel, release, oldrel-1)
* win-builder: devel

## R CMD check results
0 ERRORs, 0 WARNINGs, 0 NOTEs
