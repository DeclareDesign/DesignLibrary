test_that("designers with DesignLibrary 0.1 names return designs", {
  d <- two_arm_designer(N = 20, ate = 0.5)
  expect_s3_class(d, "design")
  est <- DeclareDesign::draw_estimands(d)
  expect_equal(as.numeric(est$estimand[est$inquiry == "ATE"]), 0.5, tolerance = 1e-8)

  d2 <- two_arm_designer(N = 20, control_mean = 1, treatment_mean = 3)
  est2 <- DeclareDesign::draw_estimands(d2)
  expect_equal(as.numeric(est2$estimand[est2$inquiry == "ATE"]), 2, tolerance = 1e-8)

  expect_s3_class(two_arm_attrition_designer(N = 30), "design")
  expect_s3_class(pretest_posttest_designer(N = 30), "design")
  expect_s3_class(randomized_response_designer(N = 40), "design")
  expect_s3_class(mediation_analysis_designer(N = 40), "design")
  expect_s3_class(multi_arm_designer(N = 30), "design")
  expect_s3_class(two_by_two_designer(N = 20, outcome_means = c(0, 0, 0, 1)), "design")
  expect_s3_class(
    block_cluster_two_arm_designer(N_blocks = 2, N_clusters_in_block = 2, N_i_in_cluster = 2),
    "design"
  )
})

test_that("multi_arm_designer accepts m_arms = 4", {
  d3 <- multi_arm_designer(N = 30)
  expect_s3_class(d3, "design")
  est3 <- DeclareDesign::draw_estimands(d3)
  expect_equal(sort(as.character(est3$inquiry)), c("ate_Y_2_1", "ate_Y_3_1", "ate_Y_3_2"))
  est3_hat <- DeclareDesign::draw_estimates(d3)
  expect_equal(est3_hat$estimator, c("DIM (Z_2 - Z_1)", "DIM (Z_3 - Z_1)", "DIM (Z_3 - Z_2)"))
  expect_equal(est3_hat$inquiry, c("ate_Y_2_1", "ate_Y_3_1", "ate_Y_3_2"))

  d4 <- multi_arm_designer(m_arms = 4)
  expect_s3_class(d4, "design")
  est4 <- DeclareDesign::draw_estimands(d4)
  expect_equal(
    sort(as.character(est4$inquiry)),
    c("ate_Y_2_1", "ate_Y_3_1", "ate_Y_3_2", "ate_Y_4_1", "ate_Y_4_2", "ate_Y_4_3")
  )

  d4b <- multi_arm_designer(
    N = 40,
    m_arms = 4,
    outcome_means = c(0, 0.5, 1, 2)
  )
  expect_s3_class(d4b, "design")
  dat <- DeclareDesign::draw_data(d4b)
  expect_equal(sort(unique(as.integer(dat$Z))), 1:4)
  est <- DeclareDesign::draw_estimands(d4b)
  expect_equal(as.numeric(est$estimand[est$inquiry == "ate_Y_2_1"]), 0.5, tolerance = 1e-8)
  expect_equal(as.numeric(est$estimand[est$inquiry == "ate_Y_4_1"]), 2, tolerance = 1e-8)
  expect_equal(as.numeric(est$estimand[est$inquiry == "ate_Y_4_2"]), 1.5, tolerance = 1e-8)

  expect_error(
    multi_arm_designer(m_arms = 4, outcome_means = c(0, 0.5, 2)),
    "outcome_means must have length m_arms"
  )

  d_lib <- make_design("multiarm_trial")
  est_lib <- DeclareDesign::draw_estimands(d_lib)
  expect_equal(as.numeric(est_lib$estimand[est_lib$inquiry == "ate_Y_2_1"]), 0, tolerance = 1e-8)
  expect_equal(as.numeric(est_lib$estimand[est_lib$inquiry == "ate_Y_3_1"]), 0, tolerance = 1e-8)
  expect_equal(attr(d_lib, "design_library_id"), "multiarm_trial")

  expect_error(make_design("multi_arm_three"), "Unknown design")

  d_make <- make_design(
    "multiarm_trial",
    N = 40,
    m_arms = 4,
    outcome_means = c(0, 0.5, 1, 2),
    outcome_sds = c(0, 0, 0, 0),
    conditions = 1:4
  )
  expect_equal(sort(unique(as.integer(DeclareDesign::draw_data(d_make)$Z))), 1:4)
})

test_that("designer argument names match DesignLibrary 0.1 where we claim them", {
  expect_true(all(
    c("N", "assignment_prob", "control_mean", "control_sd", "ate",
      "treatment_mean", "treatment_sd", "rho") %in%
      names(formals(two_arm_designer))
  ))
  expect_true(all(
    c("N", "a_R", "b_R", "a_Y", "b_Y", "rho") %in%
      names(formals(two_arm_attrition_designer))
  ))
  expect_true(all(
    c("N", "ate", "sd_1", "sd_2", "rho", "attrition_rate") %in%
      names(formals(pretest_posttest_designer))
  ))
  expect_true(all(
    c("N", "prob_forced_yes", "prevalence_rate", "withholding_rate") %in%
      names(formals(randomized_response_designer))
  ))
  expect_true(all(
    c("N", "a", "b", "c", "d", "rho") %in%
      names(formals(mediation_analysis_designer))
  ))
  expect_true(all(
    c("N", "m_arms", "outcome_means", "sd_i", "outcome_sds", "conditions", "Y") %in%
      names(formals(multi_arm_designer))
  ))
  expect_true(all(
    c("N", "prob_A", "prob_B", "weight_A", "weight_B", "outcome_means") %in%
      names(formals(two_by_two_designer))
  ))
  expect_true(all(
    c("N_blocks", "N_clusters_in_block", "N_i_in_cluster", "ate") %in%
      names(formals(block_cluster_two_arm_designer))
  ))
  expect_true(all(
    c("N", "prob", "control_mean", "sd", "ate", "h", "treatment_mean",
      "rho_WY", "rho_WZ") %in%
      names(formals(two_arm_covariate_designer))
  ))
})

test_that("factorial_designer() validates as DesignLibrary 0.1 did", {
  expect_error(factorial_designer(k = 1), "at least 2")
  expect_error(factorial_designer(outcome_means = rep(0, 4)), "2\\^k")
  expect_error(factorial_designer(assignment_probs = 0.5), "length k")
  expect_error(factorial_designer(treatment_names = c("A", "B")), "length k")
  expect_error(factorial_designer(outcome_name = "my Y"), "spaces")
})

test_that("fixed-effects cluster designs ask for CR2, as estimatr 1.0.6 gave by default", {
  skip_if_not_installed("DeclareDesign")
  skip_on_cran()

  d <- make_design("block_cluster_two_arm")
  dat <- DeclareDesign::draw_data(d)
  est <- DeclareDesign::get_estimates(d, data = dat)
  cr2 <- estimatr::lm_robust(Y ~ Z, data = dat, fixed_effects = ~ blocks,
                             clusters = clusters, se_type = "CR2")
  expect_equal(est$std.error, unname(cr2$std.error["Z"]))

  d <- make_design("stepped_wedge")
  dat <- DeclareDesign::draw_data(d)
  est <- DeclareDesign::get_estimates(d, data = dat)
  cr2 <- estimatr::lm_robust(Y ~ Z, data = dat, fixed_effects = ~ periods + units,
                             clusters = units, subset = time < max(time), se_type = "CR2")
  expect_equal(est$std.error, unname(cr2$std.error["Z"]))
})

test_that("designer defaults match DesignLibrary 0.1.10 except where documented", {
  source(test_path("fixtures", "designer_defaults_0_1_10.R"), local = TRUE)
  for (fn in names(designer_defaults_0_1_10)) {
    # A simplified signature, documented in the designer and the upgrading vignette
    if (fn == "block_cluster_two_arm_designer") next
    now <- formals(get(fn))
    old <- designer_defaults_0_1_10[[fn]]
    for (arg in names(old)) {
      # 30 in 0.1.10 did not divide into three arms of the design's blocks
      if (fn == "multi_arm_designer" && arg == "N") next
      label <- paste0(fn, "(", arg, ")")
      expect_true(arg %in% names(now), label = label)
      expect_equal(eval(now[[arg]], baseenv()), old[[arg]], label = label)
    }
  }
})

test_that("spillover_designer() builds, sweeps, and validates", {
  d <- spillover_designer(N_groups = 10, N_i_group = 3)
  expect_s3_class(d, "design")
  expect_equal(nrow(DeclareDesign::draw_data(d)), 30)
  expect_length(spillover_designer(N_groups = 10, sd_i = c(0.1, 0.2)), 2)
  expect_error(spillover_designer(sd_i = -1), "non-negative")
})

test_that("cluster_sampling_designer() samples the requested clusters and units", {
  d <- cluster_sampling_designer(
    N_clusters_in_block = 20, N_i_in_cluster = 5,
    n_clusters_in_block = 4, n_i_in_cluster = 2
  )
  expect_s3_class(d, "design")
  expect_equal(nrow(DeclareDesign::draw_data(d)), 8)
  expect_error(cluster_sampling_designer(icc = 1.5), "\\[0, 1\\]")
  expect_length(cluster_sampling_designer(N_clusters_in_block = 20, n_clusters_in_block = 4, icc = c(0.1, 0.3)), 2)
})

test_that("process_tracing_designer() checks probabilities, correlations, and labels", {
  expect_error(process_tracing_designer(N = 0), "positive integer")
  expect_error(process_tracing_designer(process_proportions = c(0.5, 0.5, 0.5, 0.5)), "sum to 1")
  expect_error(process_tracing_designer(p_E1_H = 1.2), "p_E1_H must be in")
  expect_error(process_tracing_designer(cor_E1E2_H = 1), "not compatible")
  skip_if_not_installed("CausalQueries")
  skip_if_not_installed("rdss")
  expect_warning(process_tracing_designer(label_E1 = "Hoop"), "label_E1 and label_E2 are ignored")
})
