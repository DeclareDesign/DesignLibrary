# Install DesignLibrary system dependencies

Installs package Depends and Imports (and, by default, Suggests needed
for the Shiny browser), plus any extra packages declared in design YAML
`packages:` fields. Everything comes from CRAN except `interference`
(used by `network_experiment`), which comes from GitHub via
[`remotes::install_github()`](https://remotes.r-lib.org/reference/install_github.html).
An installed `DeclareDesign`, `fabricatr`, `estimatr`, or `randomizr`
older than 2.0.0 is upgraded from CRAN.

## Usage

``` r
install_library_dependencies(
  include_shiny = TRUE,
  include_suggests = FALSE,
  ask = FALSE,
  upgrade = "never",
  verbose = TRUE,
  dry_run = FALSE
)
```

## Arguments

- include_shiny:

  If `TRUE` (default), also install Shiny Suggests (`shiny`, `bslib`,
  `htmltools`, `markdown`).

- include_suggests:

  If `TRUE`, install all DESCRIPTION Suggests. Ignored when
  `include_shiny` already covers the Shiny set unless you want test/doc
  packages too.

- ask:

  Passed to
  [`remotes::install_github()`](https://remotes.r-lib.org/reference/install_github.html).

- upgrade:

  Passed to
  [`remotes::install_github()`](https://remotes.r-lib.org/reference/install_github.html).

- verbose:

  Print progress messages.

- dry_run:

  If `TRUE`, install nothing and return the plan: `to_install` names the
  packages that would be installed or upgraded.

## Value

Invisibly, a list with `installed`, `already_ok`, `failed`, and
`to_install`. If a package that was already loaded is upgraded, restart
R before using it; a message says so.

## Details

Typical server workflow:

    install.packages("DesignLibrary")
    DesignLibrary::install_library_dependencies()
    DesignLibrary::copy_library_shiny("/path/to/shiny-app")
