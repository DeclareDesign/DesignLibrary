# Create a pretest-posttest design

Routes to
[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)
with id `"pretest_posttest"`:
`make_design("pretest_posttest", N = N, ate = ate, ...)`.

## Usage

``` r
pretest_posttest_designer(
  N = 100,
  ate = 0.25,
  sd_1 = 1,
  sd_2 = 1,
  rho = 0.5,
  attrition_rate = 0.1,
  args_to_fix = NULL
)
```

## Arguments

- N:

  Sample size.

- ate:

  Average treatment effect.

- sd_1:

  Standard deviation of the pretest shock.

- sd_2:

  Standard deviation of the posttest shock.

- rho:

  Correlation between pretest and posttest shocks (-1 to 1).

- attrition_rate:

  Share of units not observed at the posttest.

- args_to_fix:

  Ignored. Present for compatibility with DesignLibrary 0.1.

## Value

A design object.

## See also

[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)

## Examples

``` r
pretest_posttest_designer(N = 40, attrition_rate = 0.2)
#> Research design with 8 step(s):
#>   [parameters] parameters (parameters)
#>   [model] model (dgp)
#>   [inquiry] ATE (inquiry)
#>   [assignment] assignment (dgp)
#>   [measurement] measurement (dgp)
#>   [estimator] Change score (estimator)
#>   [estimator] Condition on pretest (estimator)
#>   [estimator] Posttest only (estimator)
#> 
#> Parameters and objects the design refers to:
#>            name value   kind declared steps
#>             ate  0.25 scalar     TRUE  1, 2
#>  attrition_rate   0.2 scalar     TRUE  1, 5
#>               N    40 scalar     TRUE  1, 2
#>             rho   0.5 scalar     TRUE  1, 2
#>            sd_1     1 scalar     TRUE  1, 2
#>            sd_2     1 scalar     TRUE  1, 2
```
