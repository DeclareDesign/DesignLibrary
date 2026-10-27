# Create a randomized response design

Routes to
[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)
with id `"randomized_response"`:
`make_design("randomized_response", N = N, ...)`.

## Usage

``` r
randomized_response_designer(
  N = 1000,
  prob_forced_yes = 0.6,
  prevalence_rate = 0.1,
  withholding_rate = 0.5,
  args_to_fix = NULL
)
```

## Arguments

- N:

  Sample size.

- prob_forced_yes:

  Probability a respondent is forced to answer yes.

- prevalence_rate:

  Share of units holding the sensitive trait.

- withholding_rate:

  Share of trait holders who deny it under direct questioning.

- args_to_fix:

  Ignored. Present for compatibility with DesignLibrary 0.1.

## Value

A design object.

## See also

[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)

## Examples

``` r
randomized_response_designer(N = 40, prevalence_rate = 0.2)
#> Research design with 7 step(s):
#>   [parameters] parameters (parameters)
#>   [model] model (dgp)
#>   [inquiry] true_rate (inquiry)
#>   [assignment] assignment (dgp)
#>   [measurement] measurement (dgp)
#>   [estimator] Forced Randomized Response (estimator)
#>   [estimator] Direct Question (estimator)
#> 
#> Parameters and objects the design refers to:
#>              name value   kind declared   steps
#>                 N    40 scalar     TRUE    1, 2
#>   prevalence_rate   0.2 scalar     TRUE    1, 2
#>   prob_forced_yes   0.6 scalar     TRUE 1, 4, 6
#>  withholding_rate   0.5 scalar     TRUE    1, 2
```
