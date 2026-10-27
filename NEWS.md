# DesignLibrary 2.0.0

**DesignLibrary 2.0 is the package developed as ResearchDesigns, and it supersedes DesignLibrary 0.1.** Designs are self-contained declaration files read through `make_design()`, `list_designs()`, and `get_args()`, and parameters are read from the design itself rather than from a designer function's formals. It requires DeclareDesign 2.0.

**Provenance.** The design declarations in `inst/designs/` are vouched for by the package authors. The library machinery around them (calling, listing, and displaying designs, including the Shiny app) was written with AI coding models, Claude and models used through Cursor among them.

**Code written for DesignLibrary 0.1 mostly keeps running.** Fifteen designers keep their 0.1 names and argument names (`two_arm_designer()`, `two_arm_attrition_designer()`, `pretest_posttest_designer()`, `randomized_response_designer()`, `mediation_analysis_designer()`, `multi_arm_designer()`, `two_by_two_designer()`, `block_cluster_two_arm_designer()`, `two_arm_covariate_designer()`, `regression_discontinuity_designer()`, `spillover_designer()`, `cluster_sampling_designer()`, `binary_iv_designer()`, `process_tracing_designer()`, and `factorial_designer()`), each routed to a library design, so every 0.1 designer has a 2.0 counterpart. The last seven are new library templates flattened from 0.1 (`two_arm_covariate`, `regression_discontinuity_polynomial`, `spillover`, `cluster_sampling`, `binary_iv`, `process_tracing_bayes`, and `factorial`); at the same arguments and seed, each draws what its 0.1 designer drew. The exception is `cluster_sampling` away from its defaults: `make_design()` builds the fixed population at the defaults before redesigning, so the population has the same distribution but comes from a later point in the random stream. `process_tracing_designer()` fixes its estimator labels at "Straw in the Wind" and "Smoking Gun" and warns when `label_E1` or `label_E2` asks for others. `factorial` builds its `2^k` potential outcomes, factor indicators, and inquiries inside single steps as functions of `k`, so `redesign(design, k = 4)` gives a 2^4 design; `outcome_means`, `outcome_sds`, `assignment_probs`, and `treatment_names` default to the lengths `k` implies.

## Breaking changes relative to 0.1.10

`vignette("upgrading")` works through each of these with code, and lists the ones that change results without an error.

Each removed function and argument below stops with an error that names its replacement.

- `simple_two_arm_designer()`, `simple_factorial_designer()`, `simple_iv_designer()`, and `simple_spillover_designer()` (deprecated in 0.1) are defunct. Use `two_arm_designer()`, `two_by_two_designer()`, `binary_iv_designer()`, and `spillover_designer()`, which take the same arguments.
- `get_design_code()` is defunct, and designs no longer carry the `code`, `description`, `definitions`, or `shiny_arguments` attributes. Use `get_code("<id>")` for a design's source (each designer's help page names its id), and `get_args()` or `design_info()` for its parameters.
- `match.call.defaults()` is defunct. Use `match.call()` and `formals()`.
- `block_cluster_two_arm_designer()` no longer takes `N`, `sd`, `sd_i_1`, `rho`, `control_mean`, or `treatment_mean`. Set the sample size through `N_blocks`, `N_clusters_in_block`, and `N_i_in_cluster`, the shocks through `sd_block`, `sd_cluster`, and `sd_i`, and the effect through `ate`. Its defaults are now 20 blocks of 4 clusters of 10 units, each shock with standard deviation 0.577, and `ate = 0.2` (0.1: one block of 100 single-unit clusters and `ate = 0`). `assignment_probs` takes one probability, not one per block. `verbose` is ignored with a warning.
- `multi_arm_designer()` defaults to `N = 90` (0.1: 30). As in 0.1, it declares an inquiry for every pair of arms (`ate_Y_2_1`, `ate_Y_3_1`, `ate_Y_3_2`, and so on) and estimates each by a difference in means between its two arms under labels like `"DIM (Z_2 - Z_1)"`. The contrasts between two treatment arms are kept on purpose: they are the ones a multi-arm study is most often underpowered for.
- `mediation_analysis_designer()`'s inquiries `Indirect_0` and `Indirect_1` are now `Mediator_Effect_0` and `Mediator_Effect_1`, with the same values.
- `process_tracing_designer()` keeps its estimator labels at `"Straw in the Wind"` and `"Smoking Gun"` and warns when `label_E1` or `label_E2` asks for others.
- `args_to_fix` is ignored with a warning when non-empty: every parameter stays redesignable on the returned design.
- Designs are DeclareDesign 2.0 designs. They have no separate potential outcomes or reveal steps, and some step names differ (the inquiry step of `factorial_designer()` is `"inquiry"`, not `"ATE"`). Inquiry and estimator labels are otherwise those of 0.1. Code that picks steps by name or position needs checking.
- To sweep a parameter, use `redesign(two_arm_designer(), N = c(50, 100))`. `expand_design(two_arm_designer, N = c(50, 100))` still works but is deprecated in DeclareDesign 2.0.
- Requires R 4.1 and DeclareDesign, fabricatr, estimatr, and randomizr 2.0.

**`make_design()`'s first argument is `.design`, and `get_code()`'s are `.design` and `.style`.** As `design`, it was partially matched by a parameter named `d`, so `make_design("mediation_analysis", d = 0.3)` read 0.3 as the design id and failed. A positional id is unaffected; `make_design(design = "x")` must become `make_design(.design = "x")` or `make_design("x")`. `get_code(id, style = "simple")` becomes `get_code(id, .style = "simple")`.

**Renamed from ResearchDesigns.** The S3 classes `research_designs_*` are now `design_library_*`, and the options `researchdesigns.*` are now `designlibrary.*`.

# ResearchDesigns 0.1.x

Developed as ResearchDesigns 0.1.x before the rename; that history is in the git log of the GitHub repository.
