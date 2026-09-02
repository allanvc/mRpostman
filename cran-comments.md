**mRpostman - Major release, v3.0.0**

This is a major release (CRAN currently has 1.4.0): the package went
through a deep refactoring in 2026. All commands of IMAP4rev1 and
IMAP4rev2 and every capability extension registered with IANA are now
implemented; searches are written as plain R expressions through the new
`query()` method; the attachment machinery was rebuilt on a real MIME
parser; every command runs through a single execution engine with
classed error conditions; and `use_uid`, `mute`, and `retries` became
connection-level defaults.

Breaking change, documented in NEWS.md: `use_uid` now defaults to TRUE
(UIDs are stable; sequence numbers renumber on expunge). The 14
`search_*()` methods, `AND()`/`OR()`, and the old attachment entry
points keep working as soft-deprecated spellings via the lifecycle
package.

Dependency changes: R >= 4.1.0 (the previous declaration was
inaccurate); lifecycle added to Imports; httr replaced by httr2 in
Suggests (vignette only).

## Test environments
* local Ubuntu 22.04, R 4.4.1
* Github Actions: macOS (release), Windows (release), Ubuntu (devel, release, oldrel-1)
* win-builder: R-devel, R-release, R-oldrelease

## R CMD check results
0 ERRORs, 0 WARNINGs, 0 NOTEs on all test environments above
(win-builder: Status OK, including the CRAN incoming feasibility check).

## Downstream dependencies
There are no reverse dependencies on CRAN.
