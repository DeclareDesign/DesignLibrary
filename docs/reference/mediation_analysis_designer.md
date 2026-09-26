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
