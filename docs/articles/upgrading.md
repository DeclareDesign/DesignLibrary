# Upgrading from DesignLibrary 0.1: what changes, what does not, and why

## The short version

DesignLibrary 2.0 replaces the designer functions of 0.1 with a library
of self-contained design files, read through
[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md).
It requires DeclareDesign, fabricatr, estimatr, and randomizr 2.0.

All fifteen 0.1 designers keep their names and their argument names,
with two exceptions:
[`block_cluster_two_arm_designer()`](https://declaredesign.org/r/designlibrary/reference/block_cluster_two_arm_designer.md)
loses six arguments, and three arguments (`label_E1` and `label_E2` of
[`process_tracing_designer()`](https://declaredesign.org/r/designlibrary/reference/process_tracing_designer.md),
and `args_to_fix` of every designer) are accepted but ignored with a
warning. Each now builds a library design, so `two_arm_designer(N = 40)`
returns the same design as `make_design("two_arm_flexible", N = 40)`.
Most 0.1 scripts that call a designer and diagnose the result run
unchanged.

There are two types of break:

- Removed functions and removed arguments stop with an error that names
  the replacement.

- A handful of changes do not error. These aries if there is a changed
  default, renamed inquiries, or renamed estimator terms or different
  step names. See [Porting an existing
  script](#porting-an-existing-script) for more.

## What does not change

A 0.1 script like this one runs as written. As in 0.1,
[`library(DesignLibrary)`](https://github.com/DeclareDesign/DesignLibrary)
also attaches DeclareDesign, fabricatr, estimatr, and randomizr.

``` r

library(DesignLibrary)

design <- two_arm_designer(N = 100, ate = 0.25)
set.seed(343)
draw_estimates(design)
#> # A tibble: 1 × 12
#>   term  estimate std.error statistic p.value conf.low conf.high    df outcome
#>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <dbl> <chr>  
#> 1 Z      -0.0159     0.171   -0.0929   0.926   -0.356     0.324    98 Y      
#> # ℹ 3 more variables: estimator <chr>, inquiry <chr>, estimand <dbl>
```

What carries over:

- The fifteen designer names:
  [`two_arm_designer()`](https://declaredesign.org/r/designlibrary/reference/two_arm_designer.md),
  [`two_arm_attrition_designer()`](https://declaredesign.org/r/designlibrary/reference/two_arm_attrition_designer.md),
  [`two_arm_covariate_designer()`](https://declaredesign.org/r/designlibrary/reference/two_arm_covariate_designer.md),
  [`pretest_posttest_designer()`](https://declaredesign.org/r/designlibrary/reference/pretest_posttest_designer.md),
  [`randomized_response_designer()`](https://declaredesign.org/r/designlibrary/reference/randomized_response_designer.md),
  [`mediation_analysis_designer()`](https://declaredesign.org/r/designlibrary/reference/mediation_analysis_designer.md),
  [`multi_arm_designer()`](https://declaredesign.org/r/designlibrary/reference/multi_arm_designer.md),
  [`two_by_two_designer()`](https://declaredesign.org/r/designlibrary/reference/two_by_two_designer.md),
  [`factorial_designer()`](https://declaredesign.org/r/designlibrary/reference/factorial_designer.md),
  [`block_cluster_two_arm_designer()`](https://declaredesign.org/r/designlibrary/reference/block_cluster_two_arm_designer.md),
  [`regression_discontinuity_designer()`](https://declaredesign.org/r/designlibrary/reference/regression_discontinuity_designer.md),
  [`spillover_designer()`](https://declaredesign.org/r/designlibrary/reference/spillover_designer.md),
  [`cluster_sampling_designer()`](https://declaredesign.org/r/designlibrary/reference/cluster_sampling_designer.md),
  [`binary_iv_designer()`](https://declaredesign.org/r/designlibrary/reference/binary_iv_designer.md),
  and
  [`process_tracing_designer()`](https://declaredesign.org/r/designlibrary/reference/process_tracing_designer.md).
- Their argument names and defaults, except for
  [`block_cluster_two_arm_designer()`](https://declaredesign.org/r/designlibrary/reference/block_cluster_two_arm_designer.md)
  and the default `N` of
  [`multi_arm_designer()`](https://declaredesign.org/r/designlibrary/reference/multi_arm_designer.md).
- Inquiry and estimator labels, except for the cases listed below.
- The designs themselves. Seven of the designers became new library
  files (`two_arm_covariate`, `regression_discontinuity_polynomial`,
  `spillover`, `cluster_sampling`, `binary_iv`, `process_tracing_bayes`,
  and `factorial`) flattened from their 0.1 code, and at the same
  arguments and seed each draws what its 0.1 designer drew.

## What changes, and why

### Removed functions

Four aliases that 0.1 had already deprecated are gone, along with two
helpers. In each case you get an informative message.

``` r

simple_two_arm_designer(N = 50)
#> Error in `.defunct_01()` at DesignLibrary/R/defunct.R:30:3:
#> ! `simple_two_arm_designer()` was removed in DesignLibrary 2.0.0.
#> ℹ Use `two_arm_designer()`, which takes the same arguments.
```

| Removed | Use instead |
|----|----|
| [`simple_two_arm_designer()`](https://declaredesign.org/r/designlibrary/reference/simple_two_arm_designer.md) | [`two_arm_designer()`](https://declaredesign.org/r/designlibrary/reference/two_arm_designer.md), same arguments |
| [`simple_factorial_designer()`](https://declaredesign.org/r/designlibrary/reference/simple_two_arm_designer.md) | [`two_by_two_designer()`](https://declaredesign.org/r/designlibrary/reference/two_by_two_designer.md), same arguments |
| [`simple_iv_designer()`](https://declaredesign.org/r/designlibrary/reference/simple_two_arm_designer.md) | [`binary_iv_designer()`](https://declaredesign.org/r/designlibrary/reference/binary_iv_designer.md), same arguments |
| [`simple_spillover_designer()`](https://declaredesign.org/r/designlibrary/reference/simple_two_arm_designer.md) | [`spillover_designer()`](https://declaredesign.org/r/designlibrary/reference/spillover_designer.md), same arguments |
| [`get_design_code()`](https://declaredesign.org/r/designlibrary/reference/get_design_code.md) | `get_code("<id>")` |
| [`match.call.defaults()`](https://declaredesign.org/r/designlibrary/reference/get_design_code.md) | [`match.call()`](https://rdrr.io/r/base/match.call.html) and [`formals()`](https://rdrr.io/r/base/formals.html) |

In 0.1 every designer pasted its own source code into an attribute of
the design it returned, and
[`get_design_code()`](https://declaredesign.org/r/designlibrary/reference/get_design_code.md)
read that attribute back. In 2.0 the source of a design is a file in the
package, so there is nothing to paste:
[`get_code()`](https://declaredesign.org/r/designlibrary/reference/get_code.md)
prints the file. Each designer’s help page names the id of the file it
builds.

``` r

get_design_code(two_arm_designer())
#> Error in `.defunct_01()` at DesignLibrary/R/defunct.R:65:3:
#> ! `get_design_code()` was removed in DesignLibrary 2.0.0.
#> ℹ Designs no longer carry their code as an attribute.
#> ℹ Use `get_code()` with a library design id, for example `get_code("two_arm_flexible")`.
#> ℹ `list_designs()` lists the ids, and each designer's help page names the id it builds.
```

``` r

get_code("two_arm_flexible")
#> design <-
#>   declare_parameters(
#>     N = 100,
#>     assignment_prob = 0.5,
#>     control_mean = 0,
#>     control_sd = 1,
#>     ate = 1,
#>     treatment_sd = 1,
#>     rho = 1
#>   ) +
#>   declare_model(
#>     N = N,
#>     u_0 = rnorm(N),
#>     u_1 = rnorm(n = N, mean = rho * u_0, sd = sqrt(pmax(0, 1 - rho^2))),
#>     potential_outcomes(
#>       Y ~ control_mean +
#>           (1 - Z) * (u_0 * control_sd) + Z* (u_1 * treatment_sd + ate)
#>     )
#>   ) +
#>   declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
#>   declare_assignment(Z = complete_ra(N, prob = assignment_prob)) +
#>   declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
#>   declare_estimator(Y ~ Z, inquiry = "ATE")
```

The `description`, `definitions`, and `shiny_arguments` attributes went
with `code`. Now
[`get_args()`](https://declaredesign.org/r/designlibrary/reference/get_args.md)
and
[`design_info()`](https://declaredesign.org/r/designlibrary/reference/design_info.md)
describe a design’s parameters.

### `block_cluster_two_arm_designer()` takes new arguments

The 0.1 designer derived its level sizes from `N` and its three shock
standard deviations from one total `sd`, through defaults that referred
to each other. Its defaults also produced a design with no blocking and
no clustering (one block of 100 single-unit clusters) and no effect. The
2.0 designer takes the level sizes and the shocks directly, and its
defaults are a blocked and clustered design with an effect to find: 20
blocks of 4 clusters of 10 units, each shock with standard deviation
0.577, and `ate = 0.2`.

The 0.1 arguments `N`, `sd`, `sd_i_1`, `rho`, `control_mean`, and
`treatment_mean` are errors that say what to set instead.

``` r

block_cluster_two_arm_designer(N = 200, rho = 0.5)
#> Error in `block_cluster_two_arm_designer()`:
#> ! `block_cluster_two_arm_designer()` no longer takes `N`, `rho` (DesignLibrary 0.1).
#> ℹ Set the sample size through `N_blocks`, `N_clusters_in_block`, and `N_i_in_cluster`.
#> ℹ Treatment and control potential outcomes are perfectly correlated; `rho` is not supported.
```

``` r

design <- block_cluster_two_arm_designer(
  N_blocks = 5, N_clusters_in_block = 4, N_i_in_cluster = 10, ate = 0.1
)
```

`assignment_probs` takes one probability rather than one per block, and
`verbose` is ignored with a warning. **Because the defaults changed,
[`block_cluster_two_arm_designer()`](https://declaredesign.org/r/designlibrary/reference/block_cluster_two_arm_designer.md)
with no arguments is a different design in 2.0, and nothing warns about
it.**

### `multi_arm_designer()` has a larger default sample

As in 0.1, the designer declares an inquiry for every pair of arms and
estimates each by a difference in means between its two arms, under
labels like `"DIM (Z_2 - Z_1)"`. The default `N` went from 30 to 90, so
that each of the three arms has 30 units rather than 10.

``` r

design <- multi_arm_designer(m_arms = 3)
set.seed(343)
draw_estimates(design)[, c("estimator", "term", "estimate", "inquiry")]
#> # A tibble: 3 × 4
#>   estimator       term  estimate inquiry  
#>   <chr>           <chr>    <dbl> <chr>    
#> 1 DIM (Z_2 - Z_1) Z2      -0.621 ate_Y_2_1
#> 2 DIM (Z_3 - Z_1) Z3      -0.350 ate_Y_3_1
#> 3 DIM (Z_3 - Z_2) Z3       0.271 ate_Y_3_2
```

### Two renamings that change no values

[`mediation_analysis_designer()`](https://declaredesign.org/r/designlibrary/reference/mediation_analysis_designer.md)’s
inquiries `Indirect_0` and `Indirect_1` are now `Mediator_Effect_0` and
`Mediator_Effect_1`. Each is the effect of the mediator on the outcome
with treatment held at 0 or at 1.

``` r

draw_estimands(mediation_analysis_designer())
#> # A tibble: 7 × 2
#>   inquiry             estimand
#>   <chr>                  <dbl>
#> 1 FirstStage             0.335
#> 2 Mediator_Effect_0      0.4  
#> 3 Mediator_Effect_1      0.4  
#> 4 Controlled_Direct_0    0.5  
#> 5 Controlled_Direct_1    0.5  
#> 6 Natural_Direct_0       0.5  
#> 7 Natural_Direct_1       0.5
```

[`process_tracing_designer()`](https://declaredesign.org/r/designlibrary/reference/process_tracing_designer.md)
fixes its estimator labels at `"Straw in the Wind"` and `"Smoking Gun"`.

``` r

design <- process_tracing_designer(label_E1 = "Hoop")
#> Warning: label_E1 and label_E2 are ignored; the estimator labels are fixed.
```

### `args_to_fix` is ignored

0.1 designers could fix arguments into the returned design so that
[`expand_design()`](https://rdrr.io/pkg/DeclareDesign/man/expand_design.html)
could not vary them. In 2.0 every parameter stays redesignable on the
design object, and a non-empty `args_to_fix` warns.

``` r

design <- two_arm_designer(args_to_fix = "N")
#> Warning: args_to_fix is ignored; parameters remain redesignable on the design
#> object.
```

### Designs are now all DeclareDesign 2.0 designs

DeclareDesign 2.0 has no separate potential outcomes or reveal steps:
potential outcomes live in
[`declare_model()`](https://rdrr.io/pkg/DeclareDesign/man/declare_model.html)
and are revealed in
[`declare_measurement()`](https://rdrr.io/pkg/DeclareDesign/man/declare_measurement.html).
Some step names differ as a result.
[`factorial_designer()`](https://declaredesign.org/r/designlibrary/reference/factorial_designer.md)’s
inquiry step, for example, was `"ATE"` and is now `"inquiry"`.

``` r

names(factorial_designer())
#> [1] "parameters"  "model"       "assignment"  "measurement" "inquiry"    
#> [6] "estimator"
```

Inquiry and estimator labels are otherwise those of 0.1. **Code that
picks a step by name or by position (`design[[3]]`) needs checking**,
since the position of a step can change even where its name does not.

### Sweeping a parameter

[`expand_design()`](https://rdrr.io/pkg/DeclareDesign/man/expand_design.html)
still works but is deprecated in DeclareDesign 2.0. Pass a vector to
[`redesign()`](https://rdrr.io/pkg/DeclareDesign/man/redesign.html)
instead, which needs no designer function at all.

``` r

designs <- redesign(two_arm_designer(), N = c(50, 100))
length(designs)
#> [1] 2
```

One caution:
[`process_tracing_designer()`](https://declaredesign.org/r/designlibrary/reference/process_tracing_designer.md)
checks that `cor_E1E2_H` and `cor_E1E2_not_H` are compatible with the
clue probabilities, and
[`redesign()`](https://rdrr.io/pkg/DeclareDesign/man/redesign.html) does
not. An incompatible value passed through
[`redesign()`](https://rdrr.io/pkg/DeclareDesign/man/redesign.html)
fails only when the design is run, with “negative probability” from the
measurement step.

## Porting an existing script

Search the script for each pattern in the left column.

| Search for | Replace with | If you do not |
|----|----|----|
| `simple_two_arm_designer`, `simple_factorial_designer`, `simple_iv_designer`, `simple_spillover_designer` | `two_arm_designer`, `two_by_two_designer`, `binary_iv_designer`, `spillover_designer` | error naming the replacement |
| `get_design_code(` | `get_code("<id>")` | error naming the replacement |
| `match.call.defaults(` | [`match.call()`](https://rdrr.io/r/base/match.call.html) and [`formals()`](https://rdrr.io/r/base/formals.html) | error |
| `block_cluster_two_arm_designer(` with `N`, `sd`, `sd_i_1`, `rho`, `control_mean`, or `treatment_mean` | `N_blocks`, `N_clusters_in_block`, `N_i_in_cluster`, `sd_block`, `sd_cluster`, `sd_i`, `ate` | error naming the replacement |
| [`block_cluster_two_arm_designer()`](https://declaredesign.org/r/designlibrary/reference/block_cluster_two_arm_designer.md) relying on defaults | state the 0.1 sizes and `ate = 0` explicitly | **silently a different design** |
| `multi_arm_designer(` without `N` | `N = 30` for the 0.1 default | **silently a larger sample** |
| `Indirect_0`, `Indirect_1` | `Mediator_Effect_0`, `Mediator_Effect_1` | **filter returns no rows** |
| `design[[` with a number, or step names such as `"ATE"` | `names(design)` to find the step | **wrong step, or error** |
| `args_to_fix` | drop it | warning |
| `expand_design(` | `redesign(designer(), arg = values)` | deprecation warning |
| `make_design(design =` | `make_design("<id>")` | error |

The rows in bold are the ones to search for deliberately, since nothing
will point you to them.

## What is new

A library of simple design files sit at the heart of library 2.0. The
library holds 75 designs, including the declarations from *Research
Design in the Social Sciences*.

- [`list_designs()`](https://declaredesign.org/r/designlibrary/reference/list_designs.md)
  browses the library
- [`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)
  builds a design and changes its parameters
- [`get_args()`](https://declaredesign.org/r/designlibrary/reference/get_args.md)
  lists what can be changed
- [`get_code()`](https://declaredesign.org/r/designlibrary/reference/get_code.md)
  prints the source
- [`run_shiny()`](https://declaredesign.org/r/designlibrary/reference/run_shiny.md)
  opens thelibrary in a browser
- [`vignette("DesignLibrary")`](https://declaredesign.org/r/designlibrary/articles/DesignLibrary.md)
  gives you a tour.
