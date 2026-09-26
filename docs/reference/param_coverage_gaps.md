# Objects declared before design, used by it, but missing from design params

Runs the design, reads `discover_design_params()`, and compares to
top-level assignments before `design <-`. Flags names that are used in
the design body (or seen by DeclareDesign's object finder) but not in
the redesignable parameter list.

## Usage

``` r
param_coverage_gaps(design, include_steps = FALSE)
```

## Arguments

- design:

  Design id/alias, or a parsed design list from `parse_design_file()`.

- include_steps:

  If `TRUE`, also report design pieces. Default `FALSE`
  (audit-friendly).

## Value

Data frame of gaps (possibly empty) with columns `id`, `name`, `type`,
`atomic`, `used_in_code`, `in_finder`, `in_params`.

## Details

Design steps (MIDA pieces built with `declare_*`, etc.) are not
parameters and are omitted unless `include_steps = TRUE`. Functions
assigned before `design <-` are R-only parameters.
