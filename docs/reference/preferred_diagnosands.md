# Preferred diagnosands declared in a design's YAML

Reads optional `diagnosands:` from the design frontmatter (for example
`diagnosands: [rmse, bias]` or `diagnosands: rmse, -bias, power`).
Positive names are preferred defaults for diagnosis and redesign
display. Names prefixed with `-` (e.g. `-bias`) are exclusions and are
omitted here; see
[`excluded_diagnosands()`](https://declaredesign.org/r/designlibrary/reference/excluded_diagnosands.md).

## Usage

``` r
preferred_diagnosands(design)
```

## Arguments

- design:

  Design id or book alias.

## Value

Character vector (possibly empty).

## Examples

``` r
if (FALSE) { # \dontrun{
preferred_diagnosands("two_arm_simple")
} # }
```
