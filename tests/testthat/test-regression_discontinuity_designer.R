test_designer_contract("regression_discontinuity_designer")

test_that("regression_discontinuity_designer refuses invalid arguments", {
  expect_error(regression_discontinuity_designer(cutoff = -10), "cutoff must be in (0,1).", fixed = TRUE)
  expect_error(regression_discontinuity_designer(poly_reg_order = -10), "poly_reg_order must be at least 1.", fixed = TRUE)
  expect_error(regression_discontinuity_designer(poly_reg_order = -0.1), "poly_reg_order must be at least 1.", fixed = TRUE)
  expect_error(regression_discontinuity_designer(poly_reg_order = "hello"), "poly_reg_order must be an integer.", fixed = TRUE)
  expect_error(regression_discontinuity_designer(control_coefs = NULL), "control_coefs must be a numeric vector of length > 0.", fixed = TRUE)
  expect_error(regression_discontinuity_designer(treatment_coefs = NULL), "treatment_coefs must be a numeric vector of length > 0.", fixed = TRUE)
  expect_error(regression_discontinuity_designer(outcome_sd = -1), "outcome_sd must be positive.", fixed = TRUE)
})
