# Write audit results to CSV, markdown, and plain text under `tools/`

Write audit results to CSV, markdown, and plain text under `tools/`

## Usage

``` r
write_audit_report(x, dir = NULL)
```

## Arguments

- x:

  A `design_library_audit` object from
  [`audit_designs()`](https://declaredesign.org/r/designlibrary/reference/audit_designs.md).

- dir:

  Output directory. Default: `tools/` under the package root.

## Value

Character vector of paths written (invisibly).
