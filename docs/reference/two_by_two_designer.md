# Create a two-by-two factorial design

Routes to
[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)
with id `"two_by_two"`:
`make_design("two_by_two", N = N, outcome_means = outcome_means, ...)`.

## Usage

``` r
two_by_two_designer(
  N = 100,
  prob_A = 0.5,
  prob_B = 0.5,
  weight_A = 0.5,
  weight_B = 0.5,
  outcome_means = c(0, 0, 0, 0),
  mean_A0B0 = NULL,
  mean_A0B1 = NULL,
  mean_A1B0 = NULL,
  mean_A1B1 = NULL,
  sd_i = 1,
  outcome_sds = c(0, 0, 0, 0),
  args_to_fix = NULL
)
```

## Arguments

- N:

  Sample size.

- prob_A:

  Probability of assignment to A = 1.

- prob_B:

  Probability of assignment to B = 1.

- weight_A:

  Weight on A = 1 when defining the average effect of B.

- weight_B:

  Weight on B = 1 when defining the average effect of A.

- outcome_means:

  Average outcome in each cell, length 4.

- mean_A0B0, mean_A0B1, mean_A1B0, mean_A1B1:

  Optional cell-mean overrides.

- sd_i:

  Standard deviation of the individual-level shock.

- outcome_sds:

  Extra standard deviation in each cell, length 4.

- args_to_fix:

  Ignored. Present for compatibility with DesignLibrary 0.1.

## Value

A design object.

## Details

Cell means are `outcome_means` in order AB = 00, 01, 10, 11. Scalar
`mean_A0B0` / `mean_A0B1` / `mean_A1B0` / `mean_A1B1` override those
entries when supplied.

## See also

[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)
