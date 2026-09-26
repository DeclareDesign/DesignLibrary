# Fixed DesignLibrary 0.1 names. Do not discover via *_designer: other library
# designers may be added without being DesignLibrary 0.1 wrappers.

working_designers <- c(
  "two_arm_designer",
  "two_arm_attrition_designer",
  "pretest_posttest_designer",
  "randomized_response_designer",
  "mediation_analysis_designer",
  "multi_arm_designer",
  "two_by_two_designer",
  "block_cluster_two_arm_designer",
  "two_arm_covariate_designer",
  "regression_discontinuity_designer",
  "spillover_designer",
  "cluster_sampling_designer",
  "binary_iv_designer",
  "process_tracing_designer"
)

not_ported_designers <- c(
  "factorial_designer"
)

test_that("DesignLibrary 0.1 designer names are exported", {
  exported <- getNamespaceExports("DesignLibrary")
  for (nm in c(working_designers, not_ported_designers)) {
    expect_true(nm %in% exported, info = nm)
  }
})

test_that("ported DesignLibrary 0.1 designers return a design", {
  skip_if_not_installed("DeclareDesign")
  skip_on_cran()

  call_small <- function(nm) {
    f <- get(nm, envir = asNamespace("DesignLibrary"), inherits = FALSE)
    args <- list()
    form <- names(formals(f))
    if ("N" %in% form) args$N <- 20L
    if ("N_blocks" %in% form) args$N_blocks <- 2L
    if ("N_clusters_in_block" %in% form) args$N_clusters_in_block <- 2L
    if ("N_i_in_cluster" %in% form) args$N_i_in_cluster <- 2L
    if ("n_clusters_in_block" %in% form) args$n_clusters_in_block <- 1L
    if ("n_i_in_cluster" %in% form) args$n_i_in_cluster <- 1L
    do.call(f, args)
  }

  for (nm in working_designers) {
    d <- call_small(nm)
    expect_true(inherits(d, "design"), info = nm)
    expect_true(length(d) >= 1L, info = paste(nm, "has no steps"))
    dat <- tryCatch(
      DeclareDesign::draw_data(d),
      error = function(e) e
    )
    expect_false(inherits(dat, "error"), info = paste(nm, conditionMessage(dat)))
    expect_true(is.data.frame(dat) && nrow(dat) >= 1L, info = paste(nm, "draw_data empty"))
  }
})

# Designer formals that are rewritten in the wrapper and never passed to
# make_design() (so they are not knobs on the library file):
#   two_arm_designer, two_arm_covariate_designer: treatment_mean -> ate
#   two_by_two_designer: mean_A0B0 / mean_A0B1 / mean_A1B0 / mean_A1B1
#     -> slots of outcome_means
#   block_cluster_two_arm_designer: sd_i_0 -> sd_i, assignment_probs ->
#     assignment_prob, `...` unused DesignLibrary 0.1 args
# args_to_fix is ignored on every designer and is never a file knob.
designer_to_id <- c(
  two_arm_designer = "two_arm_flexible",
  two_arm_attrition_designer = "two_arm_attrition",
  pretest_posttest_designer = "pretest_posttest",
  randomized_response_designer = "randomized_response",
  mediation_analysis_designer = "mediation_analysis",
  multi_arm_designer = "multiarm_trial",
  two_by_two_designer = "two_by_two",
  block_cluster_two_arm_designer = "block_cluster_two_arm",
  two_arm_covariate_designer = "two_arm_covariate",
  regression_discontinuity_designer = "regression_discontinuity_polynomial",
  spillover_designer = "spillover",
  cluster_sampling_designer = "cluster_sampling",
  binary_iv_designer = "binary_iv",
  process_tracing_designer = "process_tracing_bayes"
)

wrapper_only_aliases <- list(
  two_arm_designer = "treatment_mean",
  two_arm_attrition_designer = character(0),
  pretest_posttest_designer = character(0),
  randomized_response_designer = character(0),
  mediation_analysis_designer = character(0),
  multi_arm_designer = character(0),
  two_by_two_designer = c("mean_A0B0", "mean_A0B1", "mean_A1B0", "mean_A1B1"),
  block_cluster_two_arm_designer = c("sd_i_0", "assignment_probs"),
  two_arm_covariate_designer = "treatment_mean",
  regression_discontinuity_designer = character(0),
  spillover_designer = character(0),
  cluster_sampling_designer = character(0),
  binary_iv_designer = c("a_Y", "b_Y", "d_Y"),
  process_tracing_designer = c("label_E1", "label_E2")
)

# Helper functions a library file defines for its own steps. They are R-only
# knobs of the file but have no designer formal.
file_helpers <- list(
  multi_arm_designer = "per_arm",
  regression_discontinuity_designer = "po_function",
  spillover_designer = "dgp",
  cluster_sampling_designer = "dataset",
  process_tracing_designer = c(
    "joint_prob", "bayes_rule", "prior_only", "clue_posterior", "E1_only",
    "E2_only", "both_clues"
  )
)

test_that("designer formals that are passed through match library knobs", {
  skip_if_not_installed("DeclareDesign")
  skip_on_cran()

  ns <- asNamespace("DesignLibrary")
  for (nm in working_designers) {
    f <- get(nm, envir = ns, inherits = FALSE)
    form <- names(formals(f))
    passed <- setdiff(form, c(wrapper_only_aliases[[nm]], "args_to_fix", "..."))
    knobs <- get_args(designer_to_id[[nm]])$name
    missing_knobs <- setdiff(passed, knobs)
    expect_equal(
      missing_knobs,
      character(0),
      info = paste0(nm, " formals not in get_args: ", paste(missing_knobs, collapse = ", "))
    )
    extra_knobs <- setdiff(knobs, c(form, file_helpers[[nm]]))
    expect_equal(
      extra_knobs,
      character(0),
      info = paste0(nm, " get_args names not in formals: ", paste(extra_knobs, collapse = ", "))
    )
  }
})

test_that("unported DesignLibrary 0.1 designers explain the make_design() alternative", {
  for (nm in not_ported_designers) {
    f <- get(nm, envir = asNamespace("DesignLibrary"), inherits = FALSE)
    msg <- tryCatch(
      f(),
      error = function(e) conditionMessage(e)
    )
    expect_true(is.character(msg) && nzchar(msg), info = nm)
    expect_match(msg, "make_design\\(", info = nm)
    expect_match(msg, "is not in DesignLibrary 2.0", info = nm)
    expect_match(msg, "Related designs include", info = nm)
  }
})
