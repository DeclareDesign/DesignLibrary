context("Diagnose Designers")

test_that("with and without term",{
  design_1 <- two_by_two_designer(N = 500, outcome_means = c(0,0,1,2), weight_A = 0, weight_B = 0)
  design_2 <- multi_arm_designer(N = 500, m_arms = 3, outcome_means = c(0, 0, 1))
  dx <- diagnose_design(design_1, design_2, sims = 3, bootstrap_sims = FALSE)
  
  expect_true(all(c("design_1", "design_2") %in% dx$diagnosands_df$design))
  
  reshp <- reshape_diagnosis(dx)
  
})
test_that("the two designers that attached diagnosands now report the defaults", {
  # Both called DeclareDesign's set_diagnosands(), which replaces the default
  # set rather than adding to it, and does not survive redesign().
  for (design in list(process_tracing_designer(), randomized_response_designer())) {
    expect_null(attr(design, "diagnosands"))
    dx <- diagnose_design(design, sims = 3, bootstrap_sims = FALSE)$diagnosands_df
    expect_true(all(c("mean_estimand", "mean_estimate", "bias", "rmse") %in% names(dx)))
  }
})

test_that("randomized_response_designer shows the comparison it is built to make", {
  set.seed(343)
  dx <- diagnose_design(randomized_response_designer(),
                        sims = 30, bootstrap_sims = FALSE)$diagnosands_df
  bias <- setNames(dx$bias, dx$estimator)
  # The direct question is biased by design and the randomized response is not.
  # The attached diagnosands used to report `bias` alone, which showed this,
  # but only because `bias` happened to be the one diagnosand re-declared.
  expect_true(abs(bias[["Direct Question"]]) >
                abs(bias[["Forced Randomized Response"]]))
})

test_that("process_tracing_designer still diagnoses after redesign", {
  # The reason the attached set had to go: its estimators report a posterior,
  # so when redesign() dropped the attribute the defaults had no `estimate`
  # column to read and every diagnosand came back NA.
  design <- redesign(process_tracing_designer(), N = 120)
  dx <- diagnose_design(design, sims = 3, bootstrap_sims = FALSE)$diagnosands_df
  expect_false(any(is.na(dx$mean_estimate)))
  expect_false(any(is.na(dx$bias)))
})
