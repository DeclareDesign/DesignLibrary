# Defunct helpers from DesignLibrary 0.1

`get_design_code()` read the `code` attribute that DesignLibrary 0.1
designers attached to each design. Designs no longer carry that
attribute: each designer builds a library design, and
[`get_code()`](https://declaredesign.org/r/designlibrary/reference/get_code.md)
returns that design's source from its id. `match.call.defaults()` was a
helper the 0.1 designers used to write that code; use
[`match.call()`](https://rdrr.io/r/base/match.call.html) and
[`formals()`](https://rdrr.io/r/base/formals.html). Both error with a
message saying so.

## Usage

``` r
get_design_code(...)

match.call.defaults(...)
```

## Arguments

- ...:

  (optional) Ignored.

## Value

Never returns; always raises an error.
