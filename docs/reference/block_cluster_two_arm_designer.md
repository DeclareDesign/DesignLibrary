# Create a blocked and clustered two-arm design

Routes to
[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)
with id `"block_cluster_two_arm"`:
`make_design("block_cluster_two_arm", N_blocks = N_blocks, ...)`.

## Usage

``` r
block_cluster_two_arm_designer(
  N = NULL,
  N_blocks = 20,
  N_clusters_in_block = 4,
  N_i_in_cluster = 10,
  sd = NULL,
  sd_block = 0.577,
  sd_cluster = 0.577,
  sd_i = 0.577,
  sd_i_0 = NULL,
  ate = 0.2,
  assignment_prob = 0.5,
  assignment_probs = NULL,
  args_to_fix = NULL,
  ...
)
```

## Arguments

- N, sd:

  Removed DesignLibrary 0.1 arguments. Supplying either is an error that
  names the arguments to set instead.

- N_blocks:

  Number of blocks.

- N_clusters_in_block:

  Number of clusters in each block.

- N_i_in_cluster:

  Number of units in each cluster.

- sd_block:

  Standard deviation of the block-level shock.

- sd_cluster:

  Standard deviation of the cluster-level shock.

- sd_i, sd_i_0:

  Individual-level shock. `sd_i_0` is an alias.

- ate:

  Average treatment effect.

- assignment_prob, assignment_probs:

  Assignment probability. A vector of per-block probabilities is not
  supported.

- args_to_fix:

  Ignored.

- ...:

  Removed DesignLibrary 0.1 arguments, which error; any other argument
  is warned and dropped.

## Value

A design object.

## Details

A simplified signature relative to DesignLibrary 0.1: level sizes and
three shock standard deviations, a scalar assignment probability, and
`ate`. The DesignLibrary 0.1 arguments `N`, `sd`, `sd_i_1`, `rho`,
`control_mean`, and `treatment_mean` are errors that name the argument
to use instead. Other unused arguments (such as 0.1's `verbose`) are
ignored with a warning, and per-block `assignment_probs` are an error.

## See also

[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)

## Examples

``` r
block_cluster_two_arm_designer(N_blocks = 4, N_clusters_in_block = 2, N_i_in_cluster = 5)
#> Research design with 6 step(s):
#>   [parameters] parameters (parameters)
#>   [model] model (dgp)
#>   [inquiry] ATE (inquiry)
#>   [assignment] assignment (dgp)
#>   [measurement] measurement (dgp)
#>   [estimator] estimator (estimator)
#> 
#> Parameters and objects the design refers to:
#>                 name value   kind declared steps
#>      assignment_prob   0.5 scalar     TRUE  1, 4
#>                  ate   0.2 scalar     TRUE  1, 2
#>             N_blocks     4 scalar     TRUE  1, 2
#>  N_clusters_in_block     2 scalar     TRUE  1, 2
#>       N_i_in_cluster     5 scalar     TRUE  1, 2
#>             sd_block 0.577 scalar     TRUE  1, 2
#>           sd_cluster 0.577 scalar     TRUE  1, 2
#>                 sd_i 0.577 scalar     TRUE  1, 2
```
