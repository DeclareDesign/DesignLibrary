# `ate` feeds `treatment_mean` in the body.
test_designer_contract("two_arm_designer", redesign_exempt = c("ate"))

test_that("two_arm_designer refuses invalid arguments", {
  expect_error(two_arm_designer(control_sd = -1), "control_sd must be non-negative", fixed = TRUE)
  expect_error(two_arm_designer(assignment_prob = 10), "assignment_prob must be in [0,1]", fixed = TRUE)
  expect_error(two_arm_designer(rho = 10), "rho must be in [-1,1]", fixed = TRUE)
})

test_that("two_arm_designer warns when treatment_mean overrides ate", {
  expect_warning(two_arm_designer(ate = 1, control_mean = 1, treatment_mean = 1),
                 "will override `ate` value", fixed = TRUE)
})

test_that("simple_two_arm_designer is deprecated in favour of two_arm_designer", {
  expect_warning(simple_two_arm_designer(N = 10), "two_arm_designer", fixed = TRUE)
})
