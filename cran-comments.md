**mRpostman - Update to v2.3.0**

This release adds the query language: `ImapCon$query()` searches with an
ordinary R expression, e.g. `con$query((subject == "budget" | "budget 3")
& flag != "SEEN" & size > 5e6)`, captured unevaluated and translated into
an RFC 3501 search string by a pure internal function, exercised offline
by the test suite.
The criterion constructors (`string()`, `flag()`, ...) now also combine
with the native operators `&`, `|`, and `!` through an Ops group method,
with `AND()`/`OR()` unchanged. Oversized search results (an id list
larger than libcurl accepts in one line) now fail with an actionable
message recommending `esearch = TRUE`. See NEWS.md. No new dependencies.

## Test environments
* local Ubuntu 22.04, R 4.4.1
* Github Actions: macOS (release), Windows (release), Ubuntu (devel, release, oldrel-1)
* win-builder: devel

## R CMD check results
0 ERRORs, 0 WARNINGs, 0 NOTEs
