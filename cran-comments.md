## Submission

This release changes the maintainer from Jasper Cooper <jjc2247@columbia.edu> to Alexander Coppock <acoppock@gmail.com>. Jasper Cooper remains an author and will confirm the change by email.

DesignLibrary 2.0.0 is a rewrite that supersedes 0.1.10, which fails its tests on CRAN under estimatr 2.0 (ERROR on 9 of 13 check flavors). It requires DeclareDesign, fabricatr, estimatr, and randomizr 2.0, all on CRAN. The 0.1 designer functions keep their names; functions and arguments that were removed stop with an error naming the replacement, and `vignette("upgrading")` documents every change relative to 0.1.10.

## Test environments

* local macOS (Tahoe 26.6), R 4.6.1, against a library holding only CRAN releases, on commit c9b01ee: `R CMD check --as-cran`, 0 errors, 0 warnings, 1 note; tests `FAIL 0 | WARN 0 | SKIP 49 | PASS 682`.
* GitHub Actions (R-CMD-check, run 40414536514 on commit 6c34f90): macOS latest R 4.6.1; Windows latest R 4.6.1 (ucrt); Ubuntu 24.04 R 4.6.1, R 4.5.3 (oldrel-1), and R-devel (2026-10-25 r90763). All Status: OK, tests `FAIL 0 | WARN 0 | SKIP 22 | PASS 1465`.
* win-builder R-devel (2026-10-26 r90766 ucrt), Windows Server 2022, on commit 0208f78: 0 errors, 0 warnings, 1 note (the maintainer change, and 'DeclareDesign' as a possibly misspelled word). Resubmitted on c9b01ee on 2026-10-27; result pending.

## R CMD check results

0 errors | 0 warnings | 1 note

* The note is the maintainer change above. On win-builder it also lists 'DeclareDesign' in the Description as possibly misspelled; it is the name of a CRAN package and is quoted.

## Reverse dependencies

There are no reverse dependencies on CRAN.
