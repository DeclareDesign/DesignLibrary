# List designs in the library

Returns a data frame of design metadata. Printing is a grouped list of
`id (label)` lines, starting with a short getting-started sequence, then
other templates, RDSS, and remaining designs. Default listing is
metadata-only (baked library index plus a live-file overlay). The
`params` column is a comma-separated name string from that index (YAML
`params:` keys plus pre-design assignment names); designs are not
evaluated. Use
[`design_info()`](https://declaredesign.org/r/designlibrary/reference/design_info.md),
[`get_args()`](https://declaredesign.org/r/designlibrary/reference/get_args.md),
or `as.data.frame(list_designs(discover_params = TRUE))` for
redesignable parameters from a loaded design. Row order matches print
order.

## Usage

``` r
list_designs(shiny_only = FALSE, discover_params = FALSE, list_all = FALSE)
```

## Arguments

- shiny_only:

  If `TRUE`, only designs with `include_in_shiny: true` (the default
  when the field is omitted).

- discover_params:

  If `TRUE`, load each design once to list redesignable parameters
  (overrides index names). Default `FALSE` uses baked index names and
  does not evaluate designs.

- list_all:

  If `FALSE` (default), printing shows the full getting-started sequence
  and up to 10 designs in each remaining group. If `TRUE`, printing
  lists every design.

## Value

A data frame with class `design_library_list`.

## Examples

``` r
list_designs()
#> DesignLibrary library: 75 designs
#> 
#> * Getting started
#>   two_arm_simple (Simple two-arm trial)
#>   two_arm_flexible (Flexible two-arm trial)
#>   three_arm (Three-arm trial)
#>   two_by_two (2x2 factorial (library))
#>   two_arm_block_cluster (Two arm trial with blocks and clusters)
#>   two_arm_with_blocks (Two-arm trial with blocks)
#>   two_arm_attrition (Two-arm trial with attrition)
#>   factorial_2x2x2 (2x2x2 factorial)
#>   multiarm_trial (Multi-arm trial)
#>   pretest_posttest (Pretest-posttest design)
#>   randomized_response (Randomized response)
#>   mediation_analysis (Mediation analysis)
#> 
#> * Other design templates
#>   binary_iv (Binary instrumental variables)
#>   cluster_sampling (Two-stage cluster sampling)
#>   factorial (Factorial experiment)
#>   process_tracing_bayes (Process tracing with two clues)
#>   regression_discontinuity_polynomial (Regression discontinuity with a polynomial fit)
#>   spillover (Two-arm trial with spillovers within groups)
#>   two_arm_covariate (Two-arm trial with a possibly prognostic covariate)
#> 
#> * Other RDSS designs
#>   audit_experiment (audit experiment)
#>   audit_intervention (audit intervention)
#>   baseline_over_N (A baseline declaration intended to be reed over $N$.)
#>   blocked_and_clustered (blocked and clustered)
#>   bootstrapped (bootstrapped)
#>   cluster_random_sampling (cluster random sampling)
#>   conditional_expectation (Conditional expectation function)
#>   conjoint (conjoint)
#>   covariate_adjustment (covariate adjustment)
#>   declaration_using_declare (Example of declaration using Declare)
#>   ... and 46 more
#> 
#> print(list_designs(), list_all = TRUE) lists every design.
#> 
#> See design_info("id") or get_args("id") for details;
#> as.data.frame(list_designs(discover_params = TRUE)) for parameters.
list_designs(shiny_only = TRUE)
#> DesignLibrary library: 55 designs
#> 
#> * Getting started
#>   two_arm_simple (Simple two-arm trial)
#>   two_arm_flexible (Flexible two-arm trial)
#>   three_arm (Three-arm trial)
#>   two_by_two (2x2 factorial (library))
#>   two_arm_block_cluster (Two arm trial with blocks and clusters)
#>   two_arm_with_blocks (Two-arm trial with blocks)
#>   two_arm_attrition (Two-arm trial with attrition)
#>   factorial_2x2x2 (2x2x2 factorial)
#>   multiarm_trial (Multi-arm trial)
#>   pretest_posttest (Pretest-posttest design)
#>   randomized_response (Randomized response)
#>   mediation_analysis (Mediation analysis)
#> 
#> * Other design templates
#>   binary_iv (Binary instrumental variables)
#>   cluster_sampling (Two-stage cluster sampling)
#>   factorial (Factorial experiment)
#>   process_tracing_bayes (Process tracing with two clues)
#>   regression_discontinuity_polynomial (Regression discontinuity with a polynomial fit)
#>   spillover (Two-arm trial with spillovers within groups)
#>   two_arm_covariate (Two-arm trial with a possibly prognostic covariate)
#> 
#> * Other RDSS designs
#>   audit_experiment (audit experiment)
#>   audit_intervention (audit intervention)
#>   blocked_and_clustered (blocked and clustered)
#>   bootstrapped (bootstrapped)
#>   cluster_random_sampling (cluster random sampling)
#>   conjoint (conjoint)
#>   covariate_adjustment (covariate adjustment)
#>   diff_in_diff (diff-in-diff)
#>   encouragement (encouragement)
#>   factorial_2x2 (factorial 2x2)
#>   ... and 26 more
#> 
#> print(list_designs(), list_all = TRUE) lists every design.
#> 
#> See design_info("id") or get_args("id") for details;
#> as.data.frame(list_designs(discover_params = TRUE)) for parameters.
```
