# DesignLibrary

A library of declared research designs for DeclareDesign.

Version 2.0.0 supersedes DesignLibrary 0.1.10 on CRAN; see [Upgrading
from 0.1](#upgrading-from-01).

``` r

install.packages("DesignLibrary")
```

## Idea

- Each design is a self-contained `.R` file under `inst/designs/`.
- Optional YAML frontmatter adds labels, categories, keywords, book
  aliases, `packages:` extras, `diagnosands:` defaults for display, and
  `params:` tip strings.
- YAML is optional. With no header: id = filename stem (a few book
  designs set a different `id:`), label = humanized id,
  `category: Other`, object name `design`, `include_in_shiny: true`. Set
  `object:` only if the R object is not named `design`.

## Tiny API

``` r

list_designs()
make_design("two_arm_simple")
make_design("two_arm_simple", b = 0.5)
two_arm_designer(N = 40, ate = 0.2)  # DesignLibrary 0.1 name
make_design("2.1", b = 0.5)          # book alias
get_args("two_arm_simple")
get_code("two_arm_simple")             # simple make_design() + full source
run_shiny()
install_library_dependencies()        # Imports + Shiny Suggests + YAML packages
copy_library_shiny("path/to/app")     # standalone Shiny folder for the server
```

Maintainer one-stop:

``` r

refresh_library()   # index + audit + bake previews (sims = 100)
build_docs()        # pkgdown site -> docs/ (safe on Dropbox / Windows)
```

If
[`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html)
fails with `write_html` / “Error closing file”, use
[`build_docs()`](https://declaredesign.org/r/designlibrary/reference/build_docs.md)
instead: it builds in a local temp folder, then copies into `docs/`.

## Upgrading from 0.1

The fifteen 0.1 designers
([`two_arm_designer()`](https://declaredesign.org/r/designlibrary/reference/two_arm_designer.md),
[`block_cluster_two_arm_designer()`](https://declaredesign.org/r/designlibrary/reference/block_cluster_two_arm_designer.md),
and so on) keep their names and, with the exceptions below, their
arguments. Each now builds a library design, so
`two_arm_designer(N = 40)` and `make_design("two_arm_flexible", N = 40)`
return the same design.

- `simple_*_designer()`,
  [`get_design_code()`](https://declaredesign.org/r/designlibrary/reference/get_design_code.md),
  and
  [`match.call.defaults()`](https://declaredesign.org/r/designlibrary/reference/get_design_code.md)
  are defunct; each errors naming its replacement.
- [`block_cluster_two_arm_designer()`](https://declaredesign.org/r/designlibrary/reference/block_cluster_two_arm_designer.md)
  dropped `N`, `sd`, `sd_i_1`, `rho`, `control_mean`, and
  `treatment_mean` (each an error naming what to set instead) and
  changed its defaults.
- [`multi_arm_designer()`](https://declaredesign.org/r/designlibrary/reference/multi_arm_designer.md)
  compares each arm with arm 1 only;
  [`mediation_analysis_designer()`](https://declaredesign.org/r/designlibrary/reference/mediation_analysis_designer.md)
  renames `Indirect_0`/`Indirect_1` to
  `Mediator_Effect_0`/`Mediator_Effect_1`.
- redesign over a range with
  `redesign(two_arm_designer(), N = c(50, 100))`.

The vignette *Upgrading from DesignLibrary 0.1*
([`vignette("upgrading")`](https://declaredesign.org/r/designlibrary/articles/upgrading.md))
works through every break with code, and the full list is in `NEWS.md`
under “Breaking changes relative to 0.1.10”.

## Server deploy

A set of functions help you if you want to deploy the shiny to a server:

``` r

install.packages("DesignLibrary")
DesignLibrary::install_library_dependencies()
DesignLibrary::copy_library_shiny("/srv/shiny-server/designlibrary")
```

## Minimal design file

A design can be declared very concisely:

``` r

b <- 0
design <-
  declare_model(N = 100, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + b) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N)) +
  declare_measurement(Y = Y_Z_1 * Z + Y_Z_0 * (1 - Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE")
```

Save as `inst/designs/my_design.R`. Parameters are discovered from the
design; `redesign()` /
[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)
use those names.

However, if you want the design to sing in your shiny, add a YAML. YAML
can provide descriptions and definitions and can be used to set
preferred diagnosands for the Shiny Diagnosis and Redesign tabs:

``` yaml
id: two_arm_simple
label: Simple two-arm trial
category: template
keywords: [experiment, two-arm]
description: >
  Simple two-arm trial with complete random assignment and a population average treatment effect.
params:
  "N": "Number of units (sample or population size)"
  "b": "Treatment effect (outcome scale)"
diagnosands: [bias, power]
include_in_shiny: true
```

## DeclareDesign 1.x vs 2.0

**DeclareDesign 1.x** (and DesignLibrary 0.1) often paired a *designer
function* with a design: parameters lived on the designer, and tools
like `expand_design(two_arm_designer, N = c(50, 100))` swept that
function. Reproducible code was glued on with special extraction
(`{{{ }}}`).

**DeclareDesign 2.0** keeps the same declaration verbs (`declare_model`,
`+`, `diagnose_design`, …) but treats the **declared design itself** as
redesignable. Free symbols in the design (e.g. `b`, `tau`) are found on
the object, so:

``` r

designs <- redesign(design, tau = c(0.1, 0.3, 0.5))
diagnose_design(designs, sims = 100)
```

## Contributor checklist

See
[`contributor_checklist()`](https://declaredesign.org/r/designlibrary/reference/contributor_checklist.md)
or run
[`audit_designs()`](https://declaredesign.org/r/designlibrary/reference/audit_designs.md)
/
[`refresh_library()`](https://declaredesign.org/r/designlibrary/reference/refresh_library.md).
