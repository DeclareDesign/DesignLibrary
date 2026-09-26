# Refresh the library (maintainer one-stop)

Runs the contributor-facing checks and refreshes baked artifacts: index,
audits (including a short diagnosis of every design), and diagnosis
previews (`sims = 100` by default). Audit and preview failures are
reported at the end; they do not abort the refresh.

## Usage

``` r
refresh_library(sims = 100, designs = NULL, seed = 343)
```

## Arguments

- sims:

  Preview simulations (default 100).

- designs:

  Optional subset; default all for audit, shiny-on for previews.

- seed:

  Passed to
  [`bake_previews()`](https://declaredesign.org/r/designlibrary/reference/bake_previews.md).

## Value

A list with `index`, `audit`, `previews`, `ok_ids`, `preview_failures`,
and `report`.

## Details

A refresh of a subset leaves the rest of the library as it was: the
audit and refresh reports under `tools/` are rewritten only by a full
refresh, and index rows outside the subset keep the `params` column
already written.
