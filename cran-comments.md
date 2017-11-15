## Test environments
* Local Windows 10 Pro installation 10, release
* Ubuntu 14.04, oldrel, release, devel, clang and gcc with valgrind (on travis ci)
* OS X release, oldrel (on travis ci)
* Win-builder, release, devel
* Debian, R-devel, GCC ASAN/UBSAN (via r-hub)
* Debian, R-release, GCC with valgrind (via r-hub)

## R CMD check results

0 errors | 0 warnings | 0 note

## Reverse dependencies

* eulerr has no reverse dependencies.

## Hot fix

This is a hot fix for a mistake I made in 3.0.0 wherein I had omitted 
VignetteEngine{knitr::knitr} from both the vignettes, which mangled
the output (see https://cran.r-project.org/package=eulerr/vignettes/introduction.pdf
for instance). Please accept my apologies for the inconvenience and consider
this small quick fix.
