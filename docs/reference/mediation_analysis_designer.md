# Create a mediation analysis design

Routes to
[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)
with id `"mediation_analysis"`:
`make_design("mediation_analysis", N = N, a = a, ...)`.

## Usage

``` r
mediation_analysis_designer(
  N = 200,
  a = 1,
  b = 0.4,
  c = 0,
  d = 0.5,
  rho = 0,
  args_to_fix = NULL
)
```

## Arguments

- N:

  Sample size.

- a:

  Effect of treatment on the latent index for the mediator.

- b:

  Effect of the mediator on the outcome when Z = 0.

- c:

  Interaction of the mediator and treatment in the outcome.

- d:

  Direct effect of treatment on the outcome when M = 0.

- rho:

  Correlation between mediator and outcome shocks (-1 to 1).

- args_to_fix:

  Ignored. Present for compatibility with DesignLibrary 0.1.

## Value

A design object.

## See also

[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)

## Examples

``` r
mediation_analysis_designer(N = 40, a = 0.5, d = 0.2)
#> Research design with 9 step(s):
#>   [parameters] parameters (parameters)
#>   [model] model (dgp)
#>   [inquiry] inquiry (inquiry)
#>   [assignment] assignment (dgp)
#>   [measurement] measurement (dgp)
#>   [estimator] Stage 1 (estimator)
#>   [estimator] Stage 2 (estimator)
#>   [estimator] Direct_0 (estimator)
#>   [estimator] Direct_1 (estimator)
#> 
#> Parameters and objects the design refers to:
#>  name value   kind declared      steps
#>     a   0.5 scalar     TRUE       1, 2
#>     b   0.4 scalar     TRUE       1, 2
#>     c     0 scalar     TRUE 1, 2, 8, 9
#>     d   0.2 scalar     TRUE       1, 2
#>     N    40 scalar     TRUE       1, 2
#>   rho     0 scalar     TRUE       1, 2
```
