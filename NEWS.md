# DesignLibrary 0.1.4

* updated tests to be compatible with rlang 0.3.0

* fixed a bug in designers that resulted from holding all args fixed

# DesignLibrary 0.1.3

* updated code to be compatible to new DeclareDesign version on CRAN (fixes 
  a few bugs that were breaking CRAN checks)

* added dependency on `glue` for better and less `rlang`- heavy handling of strings

* `construct_design_code()` vastly improved

* changed `fixed` to `args_to_fix` and made it possible for every designer

* added definitions and other attributes to all designers to integrate better
  with external shiny app

* added some new error handling for cases that weren't handled before

* improved correlation handling in  `block_cluster_two_arm_designer()`, 
  `mediation_analysis_designer()`, and `pretest_posttest_designer()`

* updated estimands in `binary_iv_designer()`

# DesignLibrary 0.1.2

* Renamed and deprecated `simple_*` designers: 
  - `simple_two_arm_designer() -> two_arm_designer()`, 
  - `simple_spillover_designer() -> spillover_designer()`, 
  - `simple_iv_designer() -> binary_iv_designer()`, 
  - `simple_factorial_designer() -> two_by_two_designer()`  

* Added tests for new `DeclareDesign` functionality

* Updated names of `DeclareDesign` helpers (`draw_estimates`, etc.)

* Enabled specification of block-level assignment probabilities in `block_cluster_two_arm_designer()`

* Fixed a bug in `factorial_designer()` and `multi_arm_designer()` that was breaking substitution when `fixed = ` argument was used

* Small cosmetic edits to `pretest_posttest_designer()`

* Fixed estimator labels in `multi_arm_designer()`

* Small updates to warning and error messages in: 
  - `block_cluster_two_arm_designer()`
  - `cluster_sampling_designer()`

* Added PR template for contributing designers 

* Improved handling of variance in `block_cluster_two_arm_designer()`, including verbose messaging

* Imported `tidy` from `generics` following update to estimatr (>= 0.14.0)

# DesignLibrary 0.1.1

* Added designer for process-tracing designs (`process_tracing_designer()`)
* Added designer for simple instrumental variables designs (`simple_iv_designer()`)
* Added new arguments to `regression_discontinuity_designer()` to allow for setting variance and shape of potential outcomes functions
* Added blocking to `cluster_sampling_designer()`
* Added more flexible ways to specify `N` in `block_cluster_two_arm_designer()`
* Cleaned up various inconsistencies in documentation
* Cleaned up dependencies in DESCRIPTION
* Cleaner method for handling global variables in DesignLibrary.R
* Used `importFrom` to fix notes and errors in `check_results_DesignLibrary.html`

# DesignLibrary 0.1.0

* First CRAN version
