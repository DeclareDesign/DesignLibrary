# DesignLibrary

A prototype for a library of declared designs using DeclareDesign.

Version 2.0.0 supersedes DesignLibrary 0.1.10 on CRAN; see [Upgrading from 0.1](#upgrading-from-01).

```r
install.packages("DesignLibrary")
```

## Idea

- Each design is a self-contained `.R` file under `inst/designs/`.
- The **design object** is the source of truth for editable parameters.
- Optional YAML frontmatter adds labels, categories, keywords, book aliases, `packages:` extras, `diagnosands:` defaults for display, and `params:` tip strings.
- YAML is optional. With no header: id = filename stem, label = humanized id, `category: Other`, object name `design`, `include_in_shiny: true`. Set `object:` only if the R object is not named `design`.

## Tiny API

```r
list_designs()
make_design("two_arm_simple")
make_design("two_arm_simple", b = 0.5)
two_arm_designer(N = 40, ate = 0.2)  # DesignLibrary 0.1 name
make_design("2.1", b = 0.5)          # book alias
get_args("two_arm_simple")
get_code("two_arm_simple")             # simple make_design() + full source
run_shiny()
install_library_dependencies()        # Imports + Shiny Suggests + YAML packages: (+ interference from GitHub)
copy_library_shiny("path/to/app")     # standalone Shiny folder for the server
```

Maintainer one-stop:

```r
refresh_library()   # index + audit + bake previews (sims = 100)
build_docs()        # pkgdown site -> docs/ (safe on Dropbox / Windows)
```

If `pkgdown::build_site()` fails with `write_html` / "Error closing file", use `build_docs()` instead: it builds in a local temp folder, then copies into `docs/`.

## Upgrading from 0.1

The fifteen 0.1 designers (`two_arm_designer()`, `block_cluster_two_arm_designer()`, and so on) keep their names and, with the exceptions below, their arguments. Each now builds a library design, so `two_arm_designer(N = 40)` and `make_design("two_arm_flexible", N = 40)` return the same design.

- `simple_*_designer()`, `get_design_code()`, and `match.call.defaults()` are defunct; each errors naming its replacement. Use `get_code("<id>")` for a design's source.
- `block_cluster_two_arm_designer()` dropped `N`, `sd`, `sd_i_1`, `rho`, `control_mean`, and `treatment_mean` (each an error naming what to set instead) and changed its defaults.
- `multi_arm_designer()` compares each arm with arm 1 only; `mediation_analysis_designer()` renames `Indirect_0`/`Indirect_1` to `Mediator_Effect_0`/`Mediator_Effect_1`.
- Sweep with `redesign(two_arm_designer(), N = c(50, 100))`. `library(DesignLibrary)` no longer attaches DeclareDesign; building the first design does.

The vignette *Upgrading from DesignLibrary 0.1* (`vignette("upgrading")`) works through every break with code, and the full list is in `NEWS.md` under "Breaking changes relative to 0.1.10".

## Server deploy

```r
install.packages("DesignLibrary")
DesignLibrary::install_library_dependencies()
DesignLibrary::copy_library_shiny("/srv/shiny-server/designlibrary")
```

Point Shiny Server at that folder. `local.R` there is never overwritten on re-copy.
## Minimal design file

YAML is optional. This is enough:

```r
b <- 0
design <-
  declare_model(N = 100, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + b) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N)) +
  declare_measurement(Y = Y_Z_1 * Z + Y_Z_0 * (1 - Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE")
```

Save as `inst/designs/my_design.R`. Parameters are discovered from the design; `redesign()` / `make_design()` use those names.

Optional YAML can set preferred diagnosands for the Shiny Diagnosis and Redesign tabs (falls back to bias and power):

```yaml
diagnosands: [rmse, bias]
# or: diagnosands: rmse, bias
```

## Classic DeclareDesign vs DeclareDesign

**Classic DeclareDesign** (and DesignLibrary 0.1) often paired a *designer function* with a design: parameters lived on the designer, and tools like `expand_design(two_arm_designer, N = c(50, 100))` swept that function. Reproducible code was glued on with special extraction (`{{{ }}}`).

**DeclareDesign** keeps the same declaration verbs (`declare_model`, `+`, `diagnose_design`, …) but treats the **declared design itself** as redesignable. Free symbols in the design (e.g. `b`, `tau`) are found on the object, so:

```r
designs <- redesign(design, tau = c(0.1, 0.3, 0.5))
diagnose_design(designs, sims = 100)
```

needs no designer and no designer-style gluing. This package is built for that model.

## Contributor checklist

See `contributor_checklist()` or run `audit_designs()` / `refresh_library()`.
