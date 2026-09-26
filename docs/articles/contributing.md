# Contributing a design

## The short version

To add a design you only have to contribute **a single self-contained R
file** under `inst/designs/`. That file declares a design and,
preferably, a short YAML header. Once it is there,
[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md),
the maintainer audits, and
[`run_shiny()`](https://declaredesign.org/r/designlibrary/reference/run_shiny.md)
can all use it.

The contribution workflow is:

1.  **Fork** the repository and clone your fork.
2.  **Add your file** in `inst/designs/`.
3.  **Refresh** so library artifacts are built for the new design.
4.  **Check** with
    [`audit_designs()`](https://declaredesign.org/r/designlibrary/reference/audit_designs.md)
    and
    [`run_shiny()`](https://declaredesign.org/r/designlibrary/reference/run_shiny.md)
    to see it all working.
5.  Open a **pull request**.

In more detail:

## 1. Fork

Fork [DesignLibrary](https://github.com/DeclareDesign/DesignLibrary) on
GitHub and clone your fork. Work from the package source tree (or set
`options(DesignLibrary.root = "...")` so helpers find `inst/designs/`).

## 2. Add your file

Save a single `.R` file in `inst/designs/`. Name it after the design id
(for example `my_design.R`). Keep it self-contained: no
[`source()`](https://rdrr.io/r/base/source.html) of other designs.

Declare an object named `design` (or set `object:` in YAML to the object
name you use).

For example:

``` r

b <- 0.2
design <-
  declare_model(N = 100, U = rnorm(N), potential_outcomes(Y ~ b * Z + U)) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE")
```

Editable parameters are discovered from the design object. Tip strings
and library metadata live in optional YAML.

## 3. Refresh

After adding or editing a design, rebuild the index, run the audit, and
bake diagnosis previews:

``` r

options(DesignLibrary.root = "/path/to/your/DesignLibrary")
refresh_library()
```

That step creates the artifacts the library and Shiny browser expect for
your design.

## 4. Check

At minimum:

``` r

list_designs()
make_design("my_design")
audit_designs()
run_shiny()
```

Confirm the design appears in the Library, opens cleanly, and diagnosis
/ redesign behave as you expect. Use `design_info("my_design")` and
`get_args("my_design")` for a quick profile.

## 5. Pull request

Open a pull request from your fork to `macartan/DesignLibrary`. Keep the
PR focused on the new or updated design file and any packages it needs.
Mention extra dependencies in YAML `packages:` and in the PR description
when relevant.

## YAML fields

Optional frontmatter sits between `---` lines at the top of the file.

| Field | Tip |
|----|----|
| `id` | Substantive snake_case name; should match the filename stem (e.g. `two_arm_trial`). |
| `alias` | Optional book reference string (e.g. `"2.1"`). Quote it if it looks numeric. |
| `label` | Short human-readable title shown in the library. |
| `description` | One or two sentences on what the design does. Use YAML `>` for a folded block. |
| `category` | Grouping label. Use `rdss` for book designs; `template` for teaching starters; otherwise a short group name. |
| `keywords` | List of search terms, e.g. `[experiment, blocking]`. |
| `packages` | Extra R packages beyond DeclareDesign, e.g. `[margins, broom]`. |
| `diagnosands` | Preferred display diagnosands, e.g. `[rmse, bias]`. Prefix with `-` to hide one (`rmse, -bias, power`). |
| `params` | Map parameter names to tip strings. Always quote keys: `"N": "Sample size"`. Names must match redesignable parameters. Data frames and matrices are R-only (`make_design(..., dataset = ...)`), not Shiny controls. |
| `coupled` | Optional map from a driver to dependents that must match its length, e.g. `m_arms: [outcome_means, outcome_sds, conditions]`. [`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md) [`message()`](https://rdrr.io/r/base/message.html)s and the Shiny redesign help box always show the note. |
| `book_link` | URL to a book section or external docs. |
| `include_in_shiny` | `true` (default) or `false`. Set `false` for incomplete or heavy designs. |
| `object` | Name of the design object in the file. Omit if the object is named `design`. |

Example header:

``` yaml
---
id: two_arm_trial
label: Simple two-arm trial
category: template
keywords: [experiment, two-arm]
description: >
  Simple two-arm trial with complete random assignment.
params:
  "N": "Sample size"
  "b": "Treatment effect"
diagnosands: [bias, power]
include_in_shiny: true
---
```

## Checklist

``` r

library(DesignLibrary)
contributor_checklist()
#>  [1] "File lives in inst/designs/ and is self-contained (no source() of other designs)."                                                                                                                                                                                                                                                                                                              
#>  [2] "Filename matches the substantive id (e.g. two_arm_simple.R)."                                                                                                                                                                                                                                                                                                                                   
#>  [3] "YAML frontmatter is optional. If present, may set id, alias (book ref), label, category, keywords, packages, diagnosands, include_in_shiny, functional, book_link, params; object: only if the design is not named `design`."                                                                                                                                                                   
#>  [4] "No YAML is fine: id = filename stem, label = humanized id, category = Other, object = design, include_in_shiny = TRUE, functional = TRUE."                                                                                                                                                                                                                                                      
#>  [5] "Set functional: false to park a design (e.g. unavailable dependencies). Skipped by audit, smoke tests, and dependency install; forces include_in_shiny: false."                                                                                                                                                                                                                                 
#>  [6] "The design object is the source of truth for editable parameters: only names assigned before `design <-` (e.g. N <- 1000; declare_model(N = N, ...)) count. Literals inside declare_* (e.g. declare_model(N = 1000)) are not parameters."                                                                                                                                                       
#>  [7] "Scalar parameters (N <- 100) can be swept with redesign(N = c(50, 100)). Vector parameters (probs <- c(.1, .2, .3)) are replaced as a whole; the browser edits them as a comma-separated list. Data frames and matrices are package parameters (make_design(..., dataset = ...)), not Shiny controls; name the knob `dataset` (`dataset <- example_pop`), not `data` or the example's own name."
#>  [8] "YAML params map names to tip strings; always quote keys (e.g. \"N\": \"Sample size\", \"b\": \"Effect size\"); names must match those redesignable parameters (no extras). Design steps (model_*, inquiry_*, etc.) are not params."                                                                                                                                                             
#>  [9] "Optional diagnosands: preferred display diagnosands (e.g. diagnosands: rmse, bias or [rmse, bias]); prefix with - to exclude (rmse, -bias, power). Shiny Diagnosis and Redesign use these defaults."                                                                                                                                                                                            
#> [10] "Extra packages listed under packages: and available to install."                                                                                                                                                                                                                                                                                                                                
#> [11] "Design evaluates under DeclareDesign; redesign() works for documented parameters. A design that loads but does not run fails the audit."                                                                                                                                                                                                                                                        
#> [12] "Run refresh_library() from the package source tree after adding or editing designs (or set options(DesignLibrary.root = \"...\"))."
```

In R the same helpers are
[`contributor_checklist()`](https://declaredesign.org/r/designlibrary/reference/contributor_checklist.md),
[`audit_designs()`](https://declaredesign.org/r/designlibrary/reference/audit_designs.md),
and
[`refresh_library()`](https://declaredesign.org/r/designlibrary/reference/refresh_library.md).

Eight designs also have DesignLibrary 0.1 names:
[`two_arm_designer()`](https://declaredesign.org/r/designlibrary/reference/two_arm_designer.md),
[`two_arm_attrition_designer()`](https://declaredesign.org/r/designlibrary/reference/two_arm_attrition_designer.md),
[`pretest_posttest_designer()`](https://declaredesign.org/r/designlibrary/reference/pretest_posttest_designer.md),
[`randomized_response_designer()`](https://declaredesign.org/r/designlibrary/reference/randomized_response_designer.md),
[`mediation_analysis_designer()`](https://declaredesign.org/r/designlibrary/reference/mediation_analysis_designer.md),
[`multi_arm_designer()`](https://declaredesign.org/r/designlibrary/reference/multi_arm_designer.md),
[`two_by_two_designer()`](https://declaredesign.org/r/designlibrary/reference/two_by_two_designer.md),
and
[`block_cluster_two_arm_designer()`](https://declaredesign.org/r/designlibrary/reference/block_cluster_two_arm_designer.md).
