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
