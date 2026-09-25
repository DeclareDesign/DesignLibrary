# `weight_A` is inert at the defaults, where both terms of `ate_B` are zero.
test_designer_contract("two_by_two_designer", redesign_exempt = c("weight_A"))

test_that("two_by_two_designer refuses invalid arguments", {
  expect_error(two_by_two_designer(weight_A = 10), "weight_A and weight_B must be in [0,1]", fixed = TRUE)
  expect_error(two_by_two_designer(weight_B = 10), "weight_A and weight_B must be in [0,1]", fixed = TRUE)
  expect_error(two_by_two_designer(outcome_sds = -1), "sd_i and outcome_sds must be nonnegative", fixed = TRUE)
  expect_error(two_by_two_designer(prob_A = -1), "prob_ arguments must be nonnegative", fixed = TRUE)
  expect_error(two_by_two_designer(prob_A = 3), "prob_ arguments must not exceed 1", fixed = TRUE)
  expect_error(two_by_two_designer(prob_B = -1), "prob_ arguments must be nonnegative", fixed = TRUE)
  expect_error(two_by_two_designer(prob_B = 3), "prob_ arguments must not exceed 1", fixed = TRUE)
})

test_that("simple_factorial_designer is deprecated in favour of two_by_two_designer", {
  expect_warning(simple_factorial_designer(), "two_by_two_designer", fixed = TRUE)
})
