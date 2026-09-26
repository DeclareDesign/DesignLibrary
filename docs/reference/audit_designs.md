# Audit one or more designs

Checks that each file loads, exposes a design object, and that any YAML
`params:` names are a subset of the design's modifiable parameters. Also
records pre-design objects that are used by the design but missing from
the redesignable parameter list (see
[`param_coverage_report()`](https://declaredesign.org/r/designlibrary/reference/param_coverage_report.md)).

## Usage

``` r
audit_designs(designs = NULL, sims = 2, write_report = TRUE, report_dir = NULL)
```

## Arguments

- designs:

  Character vector of ids/aliases, or `NULL` for all.

- sims:

  Number of simulations for a short `diagnose_design()` (default 2). If
  `NULL`, skip the run check (load and params only).

- write_report:

  If `TRUE` (default), write CSV + markdown under `tools/`.

- report_dir:

  Directory for reports; default `tools/` under package root.

## Value

An object of class `design_library_audit`.

## Details

By default each design is diagnosed with a short `diagnose_design()` run
(`sims = 2`). A design that loads but does not run is a failure. Pass
`sims = NULL` only for a load-and-params scan while editing a single
file;
[`refresh_library()`](https://declaredesign.org/r/designlibrary/reference/refresh_library.md)
and the test suite always run the design.

Failures are collected per design and (by default) written to
`tools/audit_report.csv`, `tools/audit_report.md`, and
`tools/audit_report.txt` (FAIL/SKIP first, then OK) under the package
root via
[`write_audit_report()`](https://declaredesign.org/r/designlibrary/reference/write_audit_report.md).
Soft notes (undocumented params, coverage gaps) do not mark a design as
failed.
