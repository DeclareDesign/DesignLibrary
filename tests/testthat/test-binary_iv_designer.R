# `b_Y` and `d_Y` only fill the defaults of `b` and `d`, which the steps read instead.
test_designer_contract("binary_iv_designer", redesign_exempt = c("b_Y", "d_Y"))

test_that("binary_iv_designer refuses invalid arguments", {
  expect_error(binary_iv_designer(assignment_probs = -20), "assignment_probs must be non-negative.", fixed = TRUE)
  expect_error(binary_iv_designer(assignment_probs = 20), "assignment_probs must be < 1.", fixed = TRUE)
  expect_error(binary_iv_designer(outcome_sd = -20), "outcome_sd must be positive.", fixed = TRUE)
  expect_error(binary_iv_designer(a = -20), "vector a must be length 4.", fixed = TRUE)
  expect_error(binary_iv_designer(b = -20), "vector b must be length 4.", fixed = TRUE)
  expect_error(binary_iv_designer(d = -20), "vector d must be length 4.", fixed = TRUE)
})

test_that("simple_iv_designer is deprecated in favour of binary_iv_designer", {
  expect_warning(simple_iv_designer(), "binary_iv_designer", fixed = TRUE)
})
