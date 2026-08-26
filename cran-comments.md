**mRpostman - Update to v1.4.1**

This is a bugfix release: the fetch methods no longer fail on message parts
larger than roughly 90 kB when the `curl` package is built against
libcurl >= 8.x (see NEWS.md).

## Test environments
* local Ubuntu 22.04, R 4.4.1
* Github Actions: macOS (release), Windows (release), Ubuntu (devel, release, oldrel-1)
* win-builder: devel

## R CMD check results
0 ERRORs, 0 WARNINGs, 0 NOTEs
