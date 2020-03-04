## Test environments
* Ubuntu 19.10, R-release (on local machine)
* Ubuntu 16.04, R-release, gcc (on travis)
* Windows Server 2012 R2 x64 release (on appveyor)
* Ubuntu Linux 16.04 LTS, R-release, GCC (on rhub)
* Debian Linux, R-devel, GCC ASAN/UBSAN (on rhub)
* Fedora Linux, R-devel, clang, gfortran (on rhub)
* Win-builder, devel & release

## R CMD check results

0 errors | 0 warnings | 1 notes

> * checking CRAN incoming feasibility ... NOTE
> Maintainer: 'Johan Larsson <johanlarsson@outlook.com>'
> 
> New maintainer:
>   Johan Larsson <johanlarsson@outlook.com>
> Old maintainer(s):
>   Johan Larsson <mail@larssonjohan.com>

I am switching e-mail address for this package.

## Resubmission

This is a resubmission. In this version I have correctly resolved the
error that causes the package to fail the automated check last time.

## Reverse dependencies

* eulerr's two reverse dependencies cola and seqsetvis where checked
  by comparing R CMD checks using the development and release versions
  of eulerr (by using the package revdepcheck).
