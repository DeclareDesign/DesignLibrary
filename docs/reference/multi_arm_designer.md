# Create a multi-arm design

Routes to
[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)
with id `"multiarm_trial"`:
`make_design("multiarm_trial", N = N, m_arms = m_arms, ...)`. Defaults
for `outcome_means`, `outcome_sds`, and `conditions` are evaluated after
`m_arms`, so `multi_arm_designer(m_arms = 4)` expands those vectors
before calling
[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md).
`make_design("multiarm_trial", m_arms = 4)` does the same inside the
design: constant vectors are repeated and conditions `1..k` become
`1..m_arms`. Any other wrong-length vector errors. Inquiries are
`mean(Y(k) - Y(1))`; estimation is one `lm_robust`. Library-file knobs
match these formals.

## Usage

``` r
multi_arm_designer(
  N = 90,
  m_arms = 3,
  outcome_means = rep(0, m_arms),
  sd_i = 1,
  outcome_sds = rep(0, m_arms),
  conditions = seq_len(m_arms),
  Y = NULL,
  args_to_fix = NULL
)
```

## Arguments

- N:

  Sample size.

- m_arms:

  Number of arms. May vary. Must match `length(outcome_means)` unless
  `outcome_means` is a scalar (then recycled).

- outcome_means:

  Average outcome in each arm. Default `rep(0, m_arms)`.

- sd_i:

  Standard deviation of the individual-level shock.

- outcome_sds:

  Extra standard deviation in each arm. Default `rep(0, m_arms)`.

- conditions:

  Assignment conditions, length `m_arms`. Default `seq_len(m_arms)`.

- Y:

  Optional outcome function of `Z`, `u`, and `outcome_sds`. The library
  default is used when `Y` is `NULL`.

- args_to_fix:

  Ignored. Present for compatibility with DesignLibrary 0.1.

## Value

A design object.

## See also

[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)

## Examples

``` r
if (FALSE) { # \dontrun{
make_design("multiarm_trial", N = 90)
multi_arm_designer(m_arms = 4)
multi_arm_designer(m_arms = 4, outcome_means = c(0, 0.5, 1, 2))
} # }
```
