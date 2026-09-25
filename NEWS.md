# Changelog

## 0.1.1

Library listing in pedagogical order, eight DesignLibrary-named wrappers, parameter kinds for R and Shiny, and a two-simulation run in every audit.

**Previews at 100 simulations.** 43 of the 53 baked previews held a 2-simulation diagnosis, so the app's Diagnosis tab showed bias and power estimated from two draws while `bake_previews()` defaults to 100. All 53 are now baked at 100 (seed 343), which also brings `multilevel`'s preview up to its current estimator label.

**Two estimands fixed.** `audit_experiment` tested for `"Always-Responder"` and `"Never-Responder"` while drawing `"Always-responder"` and `"Never-responder"`, so every unit's outcomes were 1 under the white name and 0 under the Latino one and the estimate was 1 in every draw. `regression_discontinuity` centers its running variable at the cutoff but evaluated the estimand at 0.5 on that centered scale (-0.3875, where the jump at the threshold is 0.15); it is now `treatment(0) - control(0)`. Bias across 100 simulations is now 0.005 and 0.008. A test for each checks one draw's potential outcomes against the declared estimand. The previews for `audit_experiment`, `two_arm_block_cluster` and `stepped_wedge` are re-baked.

**`mediation_analysis` inquiries renamed.** `Indirect_0` and `Indirect_1` are now `Mediator_Effect_0` and `Mediator_Effect_1`. Each is the effect of the mediator on the outcome with treatment held at 0 or 1 (`b` and `b + c`), not a natural indirect effect, and the old names said otherwise. Their values are unchanged, and the preview is re-baked at 100 simulations (the one it replaces had 2).

**CR2 standard errors under estimatr 2.0.** `block_cluster_two_arm` and `stepped_wedge` now write `se_type = "CR2"` in their estimators. estimatr 1.0.6 gave CR2 by default when `fixed_effects` and `clusters` are both set; 2.0 gives CR0 and warns, which made `block_cluster_two_arm`'s SE about 16% smaller and moved its coverage and power. A test pins both designs' SEs to a direct CR2 `lm_robust()` fit.

**get_code print.** `get_code()` still returns `$simple` and `$full`. Printing uses `cat()` on `$full` (or `$simple` if `style = "simple"`) so console output is copy-paste ready rather than a quoted list.

**Library listing.** `list_designs()` prints grouped `id (label)` lines under Getting started, Other design templates, Other RDSS designs, and Other designs. Getting started is a fixed pedagogical sequence. Other groups show at most 10 designs unless `list_all = TRUE`. The Shiny library table uses that same row order (it no longer re-sorts by category then label) and no longer shows an alias column. Aliases still resolve in R, for example `make_design("2.1")`. `list_designs()` is metadata-only by default and uses the baked library index with a live-file overlay. The `params` column (and the Shiny library TOC) comes from that index — YAML `params:` keys plus pre-design assignment names — so listing does not evaluate each design.

**Getting started.** YAML labels, in list order: Simple two-arm trial; Flexible two-arm trial (library); Multi-arm trial; Two-arm trial with blocks; Two arm trial with blocks and clusters; Two-arm trial with attrition; 2x2 factorial (library); 2x2x2 factorial; Pretest-posttest design; Randomized response; Mediation analysis.

**DesignLibrary names.** Eight wrappers return a redesignable design and route to `make_design("id", ...)`. Help pages (`?two_arm_designer`, and so on) lead with that call. Argument names match DesignLibrary:

- `two_arm_designer()` (`two_arm`)
- `two_arm_attrition_designer()`
- `pretest_posttest_designer()`
- `randomized_response_designer()`
- `mediation_analysis_designer()`
- `multi_arm_designer()` (`multiarm_trial`; any `m_arms >= 2`)
- `two_by_two_designer()` (`two_by_two`)
- `block_cluster_two_arm_designer()` (simplified signature)

`args_to_fix = NULL` is silent; a non-empty value warns and is ignored. Unported names (`factorial_designer()`, `cluster_sampling_designer()`, `binary_iv_designer()`, `spillover_designer()`, `two_arm_covariate_designer()`, `regression_discontinuity_designer()`, `process_tracing_designer()`) exist as stubs: they message with related `make_design()` calls (they do not error). Compatibility tests in `tests/testthat/test-designlibrary-compatibility.R` call this known set; they do not discover functions by the `*_designer` suffix.

**Multi-arm.** The template id is `multiarm_trial` (label "Multi-arm trial"; file `inst/designs/multiarm_trial.R`). The former id `multi_arm_three` is not an alias. The exported function is still `multi_arm_designer()`. The outcome is `Y(Z)`; inquiries are `mean(Y(k) - Y(1))`; estimation is one `lm_robust` of `Y` on `factor(Z)`; arms are integers `1:m`. Designer and library defaults are `outcome_means = rep(0, m_arms)`, `outcome_sds = rep(0, m_arms)`, and `conditions = seq_len(m_arms)`. `multi_arm_designer(m_arms = 4)` expands those vectors before calling `make_design()`. `make_design("multiarm_trial", m_arms = 4)` without matching-length vectors `message()`s and still redesigns (draws may be `NaN`). YAML `coupled:` on the design documents that relationship. Library files and designers share argument names.

**make_design.** Parameter overlays (skipping `<-` assignments before eval) were reverted. `make_design()` sources the file at defaults, wraps vector replacements so `redesign()` does not treat them as a sweep (`prepare_redesign_dots()`), and applies `DeclareDesign::redesign()`.

**Coupled parameters.** Optional YAML `coupled:` maps a driver to dependents, for example `m_arms: [outcome_means, outcome_sds, conditions]`. `make_design()` `message()`s when a driver changes without matching-length dependents. `design_info()` print shows the same note. The Shiny redesign help box always shows it for designs that declare `coupled:` (not only after a failed run).

**Parameters.** `get_args()` reports `kind` (`scalar`, `vector`, `list`, `data`, or `function`) and `shiny`. A list-valued parameter (a conjoint's `levels_list`, a network experiment's `prob_exposure`) is reported and redesignable rather than dropped: `make_design(id, levels_list = x)` replaces it whole, and `make_design(id, levels_list = list(x1, x2))` sweeps, the two told apart by whether every element is itself a list. The table prints through `print.research_designs_args()`; its `default` column is a plain list rather than `I(list())`, which is what made a table holding a function unprintable. Data frames, matrices, and functions are redesignable in R (`make_design(..., data = ...)`, `make_design(..., Y = ...)`) but are not Shiny controls. In Shiny, commas on a vector replace the whole vector; a semicolon sweeps alternative vectors (for example `outcome_means: 0, 0.1, 0.2; 0.1, 0.2, 0.3`). Vector text boxes show a trailing semicolon as a sweep hint. Sampling and related designs expose a `data` argument rather than the example population name (`data <- portola` or `data <- fixed_pop` in the source). If DeclareDesign's object finder errors (empty formula names) or skips a closure, pre-design assignments are still exposed as knobs.

**Shiny.** Redesign help is a short bullet list: range sweep; vector sweep when the design has vectors; a `Note:` from YAML `coupled:` when present; R-only knobs when there are functions or data. `network_experiment` is included in the browser; adjacency and permutation matrices stay R-only.

**Audit.** `audit_designs()` diagnoses each design with `sims = 2` by default. `refresh_library()` always runs that check. A design that loads but does not run is a failure. `italian_village_bayes` uses a `summary_fn` for the Stan tidy step; `italian_village_continued` uses `estimatr::lh_robust`.

**Previews.** `bake_previews()` returns the paths it wrote. A `<<-` inside `tryCatch()` had skipped the local `paths` vector, so `refresh_library()` reported "Previews written: 0 of N" after a successful bake.

**Declarations.** New library files: `two_arm`, `two_by_two`.

**Check.** `R CMD check --as-cran` is down to the unavoidable "New submission" NOTE. `discover_design_params()` and `param_coverage_gaps()` call `DeclareDesign::design_parameters()`, the exported name for the same objects table, instead of reaching into `DeclareDesign:::find_all_objects()`. `generics` is declared in `Suggests` (`maintain.R` and the Shiny app call `generics::tidy()` behind a `requireNamespace()` guard), and `fabricatr`, `estimatr` and `randomizr` each have an `@importFrom`, since `core_packages()` attaches them by name and nothing in the package calls them with `::`. `.onLoad()` installs the design-id completion default as a `c(...)` call rather than the character vector itself, which is what the Rd `\usage` holds, so codoc no longer reports a mismatch on all five of `make_design()`, `get_args()`, `design_info()`, `get_code()` and `get_preview()`. The two `.gitkeep` files are build-ignored, and three em dashes are out of the R sources.
