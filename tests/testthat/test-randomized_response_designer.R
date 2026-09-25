test_designer_contract("randomized_response_designer")

test_that("randomized_response_designer refuses invalid arguments", {
  expect_error(randomized_response_designer(prob_forced_yes = -10), "prob_forced_yes must be in [0,1]", fixed = TRUE)
  expect_error(randomized_response_designer(prevalence_rate = -10), "prevalence_rate must be in [0,1]", fixed = TRUE)
  expect_error(randomized_response_designer(withholding_rate = -10), "withholding_rate must be in [0,1]", fixed = TRUE)
})

test_that("randomized_response_designer reports the default diagnosands", {
  # Its re-declared `bias` replaced the default set rather than adding to it.
  design <- randomized_response_designer()
  expect_null(attr(design, "diagnosands"))
  dx <- diagnose_design(design, sims = 3, bootstrap_sims = FALSE)$diagnosands_df
  expect_in(c("mean_estimand", "mean_estimate", "bias", "rmse"), names(dx))
})

test_that("randomized_response_designer shows the comparison it is built to make", {
  set.seed(343)
  dx <- diagnose_design(randomized_response_designer(),
                        sims = 30, bootstrap_sims = FALSE)$diagnosands_df
  bias <- setNames(dx$bias, dx$estimator)
  # The direct question is biased by design and the randomized response is not.
  expect_gt(abs(bias[["Direct Question"]]), abs(bias[["Forced Randomized Response"]]))
})
