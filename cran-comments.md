**mRpostman - Patch release, v3.0.1**

This is a bug-fix release (CRAN currently has 3.0.0). No API changes.

The main fix concerns GitHub issue #18: on Linux builds linked against
libcurl 8.17.0 or later, every message fetch came back empty, because
those libcurl versions hand back the literal part of a custom IMAP
command reply through the body callback instead of the header callback,
and may end the transfer before the tagged line. The package now
rebuilds every reply in wire order from both callbacks and no longer
depends on which libcurl version is linked (verified against libcurl
8.14.1, 8.18.0 and 8.23.0). The other fixes are a quadratic slowdown on
long command runs over one connection, a `thread()` parser bug, an
automatic raw-socket fallback for `sort()`/`thread()` on very large
folders, and three corrections to the `enron_sandbox()` manifest
(including a Windows path issue). Details in NEWS.md.

## Test environments
* local Ubuntu 22.04, R 4.4.1 (R CMD check --as-cran)
* Github Actions: macOS (release), Windows (release), Ubuntu (devel, release, oldrel-1)

## R CMD check results
0 ERRORs, 0 WARNINGs, 0 NOTEs on Github Actions. The local check gives
1 WARNING and 1 NOTE that belong to the local machine, not to the
package: "A complete check needs the 'checkbashisms' script" (the script
is not installed there; the configure and cleanup scripts are plain POSIX
sh) and "unable to verify current time".

## Downstream dependencies
There are no reverse dependencies on CRAN.
