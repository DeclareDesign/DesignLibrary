# Create a two-arm design with attrition

Routes to
[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)
with id `"two_arm_attrition"`:
`make_design("two_arm_attrition", N = N, a_R = a_R, ...)`.

## Usage

``` r
two_arm_attrition_designer(
  N = 100,
  a_R = 0,
  b_R = 1,
  a_Y = 0,
  b_Y = 1,
  rho = 0,
  args_to_fix = NULL
)
```

## Arguments

- N:

  Sample size.

- a_R:

  Constant in the equation relating treatment to reporting.

- b_R:

  Slope relating treatment to reporting.

- a_Y:

  Constant in the equation relating treatment to the outcome.

- b_Y:

  Slope relating treatment to the outcome.

- rho:

  Correlation between reporting and outcome shocks (0 to 1).

- args_to_fix:

  Ignored. Present for compatibility with DesignLibrary 0.1.

## Value

A design object.

## See also

[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)
