test_designer_contract("mediation_analysis_designer")

test_that("mediation_analysis_designer refuses invalid arguments", {
  expect_error(mediation_analysis_designer(rho = 10), "rho must be in [-1, 1]", fixed = TRUE)
})
