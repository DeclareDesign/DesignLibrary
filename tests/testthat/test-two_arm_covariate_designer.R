# `ate` feeds `treatment_mean` in the body.
test_designer_contract("two_arm_covariate_designer", redesign_exempt = c("ate"))

test_that("two_arm_covariate_designer refuses invalid arguments", {
  expect_error(two_arm_covariate_designer(sd = -1), "sd must be non-negative", fixed = TRUE)
  expect_error(two_arm_covariate_designer(prob = 10), "prob must be in [0,1]", fixed = TRUE)
  expect_error(two_arm_covariate_designer(rho_WY = 10), "rho_WY must be in [-1,1]", fixed = TRUE)
  expect_error(two_arm_covariate_designer(rho_WZ = 10), "rho_WZ must be in [-1,1]", fixed = TRUE)
})
