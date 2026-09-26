# Build an in-memory index of all designs

Shared by
[`list_designs()`](https://declaredesign.org/r/designlibrary/reference/list_designs.md)
(metadata-only default) and maintainer tools. When `use_cache = TRUE`,
reads the baked `inst/library_index` artifact if live `.R` filenames
match, and YAML-parses only extra, missing, or newer files so a local
`inst/designs/my_design.R` appears without refresh.

## Usage

``` r
make_index(use_cache = TRUE)
```

## Arguments

- use_cache:

  If `TRUE` (default), use the baked index with a live-file overlay. If
  `FALSE`, parse YAML from every design file (used by
  [`refresh_library()`](https://declaredesign.org/r/designlibrary/reference/refresh_library.md)).

## Value

A data frame (same columns as
[`list_designs()`](https://declaredesign.org/r/designlibrary/reference/list_designs.md),
plus description). `params` is a comma-separated name string from the
baked index, or from YAML `params:` keys plus pre-design assignment
names for overlay files.
