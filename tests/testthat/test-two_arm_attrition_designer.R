test_designer_contract("two_arm_attrition_designer")

test_that("two_arm_attrition_designer refuses invalid arguments", {
  expect_error(two_arm_attrition_designer(rho = 10), "rho must be in [0,1]", fixed = TRUE)
})
