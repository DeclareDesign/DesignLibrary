# Fixed DesignLibrary names. Do not discover via *_designer: other library
# designers may be added without being DesignLibrary wrappers.

working_designers <- c(
  "two_arm_designer",
  "two_arm_attrition_designer",
  "pretest_posttest_designer",
  "randomized_response_designer",
  "mediation_analysis_designer",
  "multi_arm_designer",
  "two_by_two_designer",
  "block_cluster_two_arm_designer"
)

not_ported_designers <- c(
  "binary_iv_designer",
  "cluster_sampling_designer",
  "factorial_designer",
  "process_tracing_designer",
  "regression_discontinuity_designer",
  "spillover_designer",
  "two_arm_covariate_designer"
)

test_that("DesignLibrary designer names are exported", {
  exported <- getNamespaceExports("ResearchDesigns")
  for (nm in c(working_designers, not_ported_designers)) {
    expect_true(nm %in% exported, info = nm)
  }
})

test_that("ported DesignLibrary designers return a design", {
  skip_if_not_installed("DeclareDesign")
  skip_on_cran()

  call_small <- function(nm) {
    f <- get(nm, envir = asNamespace("ResearchDesigns"), inherits = FALSE)
    args <- list()
    form <- names(formals(f))
    if ("N" %in% form) args$N <- 20L
    if ("N_blocks" %in% form) args$N_blocks <- 2L
    if ("N_clusters_in_block" %in% form) args$N_clusters_in_block <- 2L
    if ("N_i_in_cluster" %in% form) args$N_i_in_cluster <- 2L
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
#   two_arm_designer: treatment_mean -> ate
#   two_by_two_designer: mean_A0B0 / mean_A0B1 / mean_A1B0 / mean_A1B1
#     -> slots of outcome_means
#   block_cluster_two_arm_designer: sd_i_0 -> sd_i, assignment_probs ->
#     assignment_prob, `...` unused DesignLibrary args
# args_to_fix is ignored on every designer and is never a file knob.
designer_to_id <- c(
  two_arm_designer = "two_arm_flexible",
  two_arm_attrition_designer = "two_arm_attrition",
  pretest_posttest_designer = "pretest_posttest",
  randomized_response_designer = "randomized_response",
  mediation_analysis_designer = "mediation_analysis",
  multi_arm_designer = "multiarm_trial",
  two_by_two_designer = "two_by_two",
  block_cluster_two_arm_designer = "block_cluster_two_arm"
)

wrapper_only_aliases <- list(
  two_arm_designer = "treatment_mean",
  two_arm_attrition_designer = character(0),
  pretest_posttest_designer = character(0),
  randomized_response_designer = character(0),
  mediation_analysis_designer = character(0),
  multi_arm_designer = character(0),
  two_by_two_designer = c("mean_A0B0", "mean_A0B1", "mean_A1B0", "mean_A1B1"),
  block_cluster_two_arm_designer = c("sd_i_0", "assignment_probs")
)

# Helper functions a library file defines for its own steps. They are R-only
# knobs of the file but have no designer formal.
file_helpers <- list(multi_arm_designer = "per_arm")

test_that("designer formals that are passed through match library knobs", {
  skip_if_not_installed("DeclareDesign")
  skip_on_cran()

  ns <- asNamespace("ResearchDesigns")
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

test_that("unported DesignLibrary designers explain the make_design() alternative", {
  for (nm in not_ported_designers) {
    f <- get(nm, envir = asNamespace("ResearchDesigns"), inherits = FALSE)
    msg <- tryCatch(
      f(),
      error = function(e) conditionMessage(e)
    )
    expect_true(is.character(msg) && nzchar(msg), info = nm)
    expect_match(msg, "make_design\\(", info = nm)
    expect_match(msg, "not in ResearchDesigns", info = nm)
    expect_match(msg, "Related designs include", info = nm)
  }
})
