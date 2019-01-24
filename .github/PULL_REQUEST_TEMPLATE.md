# Description

Please include a summary of the design or designer here.

Please note, designers are functions that generate designs, and will be added to the DesignLibrary package, to be eventually merged into CRAN. Designs should be submitted as .Rmd files that will be added to the table at [DeclareDesign.org/library](DeclareDesign.org/library) but not to the package.

# Checklist for designers:

- [ ] Designer is documented according to the [contributing guidelines for designers](https://declaredesign.org/library/articles/how_to_write_and_contribute_designers.html) 
- [ ] Designer and any vignettes have been added to overview.csv
- [ ] Tests added to testthat to ensure coverage remains at 100%
- [ ] Branch is passing devtools::check() with 0 errors, warnings, or notes

# Checklist for designs:

- [ ] Design is created using most up-to-date versions of packages
- [ ] Design can be passed to `draw_data()` and `diagnose_design()` without errors
- [ ] Design is documented according to the [contributing guidelines for designs](https://declaredesign.org/library/articles/how_to_write_and_contribute_designs.html) 
- [ ] Meta-data added to overview.csv
- [ ] Any data that is required is sourced in from public URLs
- [ ] Branch is passing devtools::check() with 0 errors, warnings, or notes

