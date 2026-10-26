## Submission

This release changes the maintainer from Jasper Cooper <jjc2247@columbia.edu> to Alexander Coppock <acoppock@gmail.com>. Jasper Cooper remains an author and will confirm the change by email.

DesignLibrary 2.0.0 is a rewrite that supersedes 0.1.10, which fails its tests on CRAN under estimatr 2.0. It requires DeclareDesign, fabricatr, estimatr, and randomizr 2.0, all on CRAN. The 0.1 designer functions keep their names; functions and arguments that were removed stop with an error naming the replacement, and `vignette("upgrading")` documents every change relative to 0.1.10.

## Test environments

* local macOS (Tahoe 26.6), R 4.6.1, against a library holding only CRAN releases: `R CMD check --as-cran`, 0 errors, 0 warnings, 1 note.

## R CMD check results

0 errors | 0 warnings | 1 note

* The note is the maintainer change above.

## Reverse dependencies

There are no reverse dependencies on CRAN.
