# `k` fixes the number of factors, and so the steps themselves.
test_designer_contract("factorial_designer", redesign_exempt = c("k"))

test_that("factorial_designer refuses invalid arguments", {
  expect_error(factorial_designer(outcome_name = c("Y ")), "Please remove spaces from `outcome_name' strings.", fixed = TRUE)
  expect_error(factorial_designer(outcome_means = 1, k = 2), "arguments must be the same as length of 2^(k).", fixed = TRUE)
  expect_error(factorial_designer(outcome_sds = 1, k = 2), "arguments must be the same as length of 2^(k).", fixed = TRUE)
  expect_error(factorial_designer(treatment_names = "A", k = 2), "Length of `treatment_names` must be the same as length of k.", fixed = TRUE)
  expect_error(factorial_designer(assignment_probs = 0.5, k = 2), "`assignment_probs` must be the same as length of k.", fixed = TRUE)
  expect_error(factorial_designer(assignment_probs = 0.5, k = 1), "`k' should be a positive integer > 1.", fixed = TRUE)
  expect_error(factorial_designer(outcome_sds = c(-1, -1, -1, -1), k = 2), "`outcome_sds' should be nonnegative.", fixed = TRUE)
  expect_error(factorial_designer(assignment_probs = c(-0.5, 0.5), k = 2), "`assignment_probs' should have positive values only.", fixed = TRUE)
})
