## Test environments
* Local Antergos 18.4, release
* Ubuntu 14.04, oldrel, release, devel, gcc and clang with valgrind (on travis ci)
* OS X release, oldrel (on travis ci)
* Windows Server 2012 R2 x64 oldrel, release, patched, devel (on appveyor)
* Win-builder, release, devel

## R CMD check results

0 errors | 0 warnings | 0 note

## Sanitizer warnings

The package incurs several warnings when ran through the
GCC ASAN/UBSAN sanitizers on rhub. I believe these to be related
to the RcppParallel package
(https://cran.r-project.org/web/checks/check_results_RcppParallel.html)
and to be harmless (see https://github.com/RcppCore/RcppParallel/issues/36).

## Reverse dependencies

* eulerr has no reverse dependencies.
