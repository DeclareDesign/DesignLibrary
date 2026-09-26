# List designs in the library

Returns a data frame of design metadata. Printing is a grouped list of
`id (label)` lines, starting with a short getting-started sequence, then
other templates, RDSS, and remaining designs. Default listing is
metadata-only (baked library index plus a live-file overlay). The
`params` column is a comma-separated name string from that index (YAML
`params:` keys plus pre-design assignment names); designs are not
evaluated. Use
[`design_info()`](https://declaredesign.org/r/designlibrary/reference/design_info.md),
[`get_args()`](https://declaredesign.org/r/designlibrary/reference/get_args.md),
or `as.data.frame(list_designs(discover_params = TRUE))` for
redesignable parameters from a loaded design. Row order matches print
order.

## Usage

``` r
list_designs(shiny_only = FALSE, discover_params = FALSE, list_all = FALSE)
```

## Arguments

- shiny_only:

  If `TRUE`, only designs with `include_in_shiny: true` (the default
  when the field is omitted).

- discover_params:

  If `TRUE`, load each design once to list redesignable parameters
  (overrides index names). Default `FALSE` uses baked index names and
  does not evaluate designs.

- list_all:

  If `FALSE` (default), printing shows the full getting-started sequence
  and up to 10 designs in each remaining group. If `TRUE`, printing
  lists every design.

## Value

A data frame with class `design_library_list`.
