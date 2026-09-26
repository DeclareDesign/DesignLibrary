# DesignLibrary 0.1 designers not ported as-is

These names exist so that code written for DesignLibrary 0.1 fails with
an error that says what to write instead, rather than with "object not
found" or, worse, several lines later on a `NULL` design. Each names the
related declarations available through
[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md),
for example `make_design("encouragement")` or
`make_design("factorial_2x2")`.

## Usage

``` r
binary_iv_designer(...)

cluster_sampling_designer(...)

factorial_designer(...)

process_tracing_designer(...)

regression_discontinuity_designer(...)

spillover_designer(...)

two_arm_covariate_designer(...)
```

## Arguments

- ...:

  Ignored.

## Value

Nothing: these always stop.

## See also

[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)
