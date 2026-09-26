# Package index

## Discover

Browse the design library.

- [`list_designs()`](https://declaredesign.org/r/designlibrary/reference/list_designs.md)
  : List designs in the library
- [`design_info()`](https://declaredesign.org/r/designlibrary/reference/design_info.md)
  : Design metadata (YAML + defaults)
- [`preferred_diagnosands()`](https://declaredesign.org/r/designlibrary/reference/preferred_diagnosands.md)
  : Preferred diagnosands declared in a design's YAML
- [`excluded_diagnosands()`](https://declaredesign.org/r/designlibrary/reference/excluded_diagnosands.md)
  : Diagnosands excluded from display by YAML

## Build & inspect

Load designs, change parameters, and read code.

- [`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)
  : Build a design, optionally with redesigned parameters

- [`get_args()`](https://declaredesign.org/r/designlibrary/reference/get_args.md)
  : Editable parameters for a design

- [`print(`*`<design_library_args>`*`)`](https://declaredesign.org/r/designlibrary/reference/print.design_library_args.md)
  : Print the parameter table from get_args()

- [`get_code()`](https://declaredesign.org/r/designlibrary/reference/get_code.md)
  :

  Code for a design: simple
  [`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)
  call and/or full source

- [`design_profile()`](https://declaredesign.org/r/designlibrary/reference/design_profile.md)
  :

  Character profile for a design (same prose as printing
  [`design_info()`](https://declaredesign.org/r/designlibrary/reference/design_info.md))

## Shiny

Browse designs interactively and deploy a standalone app folder.

- [`run_shiny()`](https://declaredesign.org/r/designlibrary/reference/run_shiny.md)
  : Launch the DesignLibrary Shiny browser
- [`copy_library_shiny()`](https://declaredesign.org/r/designlibrary/reference/copy_library_shiny.md)
  : Copy the bundled Shiny app to a standalone folder
- [`install_library_dependencies()`](https://declaredesign.org/r/designlibrary/reference/install_library_dependencies.md)
  : Install DesignLibrary system dependencies
- [`get_preview()`](https://declaredesign.org/r/designlibrary/reference/get_preview.md)
  : Load a baked diagnosis preview
- [`has_preview()`](https://declaredesign.org/r/designlibrary/reference/has_preview.md)
  : Whether a baked preview exists for a design

## Maintain

Index, audit, bake previews, refresh the library, and build the site.

- [`refresh_library()`](https://declaredesign.org/r/designlibrary/reference/refresh_library.md)
  : Refresh the library (maintainer one-stop)

- [`make_index()`](https://declaredesign.org/r/designlibrary/reference/make_index.md)
  : Build an in-memory index of all designs

- [`audit_designs()`](https://declaredesign.org/r/designlibrary/reference/audit_designs.md)
  : Audit one or more designs

- [`write_audit_report()`](https://declaredesign.org/r/designlibrary/reference/write_audit_report.md)
  :

  Write audit results to CSV, markdown, and plain text under `tools/`

- [`bake_previews()`](https://declaredesign.org/r/designlibrary/reference/bake_previews.md)
  : Bake compact diagnosis previews into inst/previews

- [`build_docs()`](https://declaredesign.org/r/designlibrary/reference/build_docs.md)
  : Build the pkgdown site (Dropbox-safe)

- [`contributor_checklist()`](https://declaredesign.org/r/designlibrary/reference/contributor_checklist.md)
  : Contributor checklist for a design

- [`param_coverage_report()`](https://declaredesign.org/r/designlibrary/reference/param_coverage_report.md)
  : Report declared-before-design objects missing from design parameters

- [`param_coverage_gaps()`](https://declaredesign.org/r/designlibrary/reference/param_coverage_gaps.md)
  : Objects declared before design, used by it, but missing from design
  params

## DesignLibrary 0.1 designers

Designer functions with their DesignLibrary 0.1 names and arguments.
Working ports call
[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md);
unported names are stubs that point to related library designs.

### Working designers

- [`two_arm_designer()`](https://declaredesign.org/r/designlibrary/reference/two_arm_designer.md)
  : Create a one-level two-arm design
- [`two_arm_attrition_designer()`](https://declaredesign.org/r/designlibrary/reference/two_arm_attrition_designer.md)
  : Create a two-arm design with attrition
- [`pretest_posttest_designer()`](https://declaredesign.org/r/designlibrary/reference/pretest_posttest_designer.md)
  : Create a pretest-posttest design
- [`randomized_response_designer()`](https://declaredesign.org/r/designlibrary/reference/randomized_response_designer.md)
  : Create a randomized response design
- [`mediation_analysis_designer()`](https://declaredesign.org/r/designlibrary/reference/mediation_analysis_designer.md)
  : Create a mediation analysis design
- [`multi_arm_designer()`](https://declaredesign.org/r/designlibrary/reference/multi_arm_designer.md)
  : Create a multi-arm design
- [`two_by_two_designer()`](https://declaredesign.org/r/designlibrary/reference/two_by_two_designer.md)
  : Create a two-by-two factorial design
- [`block_cluster_two_arm_designer()`](https://declaredesign.org/r/designlibrary/reference/block_cluster_two_arm_designer.md)
  : Create a blocked and clustered two-arm design

### Unported stubs

- [`binary_iv_designer()`](https://declaredesign.org/r/designlibrary/reference/designers-not-ported.md)
  [`cluster_sampling_designer()`](https://declaredesign.org/r/designlibrary/reference/designers-not-ported.md)
  [`factorial_designer()`](https://declaredesign.org/r/designlibrary/reference/designers-not-ported.md)
  [`process_tracing_designer()`](https://declaredesign.org/r/designlibrary/reference/designers-not-ported.md)
  [`regression_discontinuity_designer()`](https://declaredesign.org/r/designlibrary/reference/designers-not-ported.md)
  [`spillover_designer()`](https://declaredesign.org/r/designlibrary/reference/designers-not-ported.md)
  [`two_arm_covariate_designer()`](https://declaredesign.org/r/designlibrary/reference/designers-not-ported.md)
  : DesignLibrary 0.1 designers not ported as-is
