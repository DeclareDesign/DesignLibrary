# Bake compact diagnosis previews into inst/previews

Each design is baked independently. Failures are collected and returned;
they do not abort the rest of the bake.

## Usage

``` r
bake_previews(designs = NULL, sims = 100, seed = 343)
```

## Arguments

- designs:

  Ids/aliases, or `NULL` for shiny-included designs.

- sims:

  Number of simulations (package default is 100).

- seed:

  Seed set before each design is built and diagnosed, so a preview does
  not depend on which other designs were baked with it. The caller's
  random number stream is restored on exit. `NULL` leaves the stream
  alone.

## Value

Invisibly, a list with `paths` (character) and `failures` (data frame
with `id` and `error`).
