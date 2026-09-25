test_that("DesignLibrary-named designers return designs", {
  skip_if_not_installed("DeclareDesign")
  skip_on_cran()

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
  skip_if_not_installed("DeclareDesign")
  skip_on_cran()

  d3 <- multi_arm_designer(N = 30)
  expect_s3_class(d3, "design")
  est3 <- DeclareDesign::draw_estimands(d3)
  expect_equal(sort(as.character(est3$inquiry)), c("ate_Y_2_1", "ate_Y_3_1"))

  d4 <- multi_arm_designer(m_arms = 4)
  expect_s3_class(d4, "design")
  est4 <- DeclareDesign::draw_estimands(d4)
  expect_equal(
    sort(as.character(est4$inquiry)),
    c("ate_Y_2_1", "ate_Y_3_1", "ate_Y_4_1")
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

  expect_error(
    multi_arm_designer(m_arms = 4, outcome_means = c(0, 0.5, 2)),
    "outcome_means must have length m_arms"
  )

  d_lib <- make_design("multiarm_trial")
  est_lib <- DeclareDesign::draw_estimands(d_lib)
  expect_equal(as.numeric(est_lib$estimand[est_lib$inquiry == "ate_Y_2_1"]), 0, tolerance = 1e-8)
  expect_equal(as.numeric(est_lib$estimand[est_lib$inquiry == "ate_Y_3_1"]), 0, tolerance = 1e-8)
  expect_equal(attr(d_lib, "research_designs_id"), "multiarm_trial")

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

test_that("designer argument names match DesignLibrary where we claim them", {
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
})

test_that("unported DesignLibrary designers stop and point at make_design()", {
  # They used to message and return invisible(NULL), so `design <-
  # factorial_designer(k = 3)` failed further downstream with an error naming
  # neither the designer nor its replacement.
  expect_error(factorial_designer(), "make_design\\(\"factorial_2x2\"\\)")
  expect_error(cluster_sampling_designer(), "cluster_random_sampling")
  expect_error(two_arm_covariate_designer(), "covariate_adjustment")
  expect_error(binary_iv_designer(), "encouragement")
  expect_error(spillover_designer(), "randomized_saturation")
  expect_error(regression_discontinuity_designer(), "regression_discontinuity")
  expect_error(process_tracing_designer(), "process_tracing")
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
