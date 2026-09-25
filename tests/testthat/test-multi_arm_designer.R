test_designer_contract("multi_arm_designer")

test_that("multi_arm_designer refuses invalid arguments", {
  expect_error(multi_arm_designer(outcome_means = rep(1, 2), m_arms = 10), "outcome_means, outcome_sds and conditions arguments must be of length m_arms.", fixed = TRUE)
  expect_error(multi_arm_designer(m_arms = 0.5, outcome_means = 2), "m_arms should be an integer greater than one.", fixed = TRUE)
  expect_error(multi_arm_designer(outcome_sds = c(-10, -10), outcome_means = c(2, 2), m_arms = 2), "outcome_sds should be nonnegative", fixed = TRUE)
  expect_error(multi_arm_designer(sd_i = -1), "sd_i should be nonnegative", fixed = TRUE)
})

test_that("multi_arm_designer draws potential outcomes around outcome_means", {
  # 0.1.10 ignored outcome_means and centred every arm at 0.
  set.seed(343)
  dat <- draw_data(multi_arm_designer(N = 3000, m_arms = 3, outcome_means = c(1, 2, 3)))
  expect_equal(unname(colMeans(dat[c("Y_Z_1", "Y_Z_2", "Y_Z_3")])), c(1, 2, 3), tolerance = 0.05)
})

test_that("multi_arm_designer fixes outcome_means when asked", {
  design <- multi_arm_designer(m_arms = 3, outcome_means = c(1, 2, 3), args_to_fix = "outcome_means")
  code <- paste(attr(design, "code"), collapse = "\n")
  expect_match(code, "(3 + u_3)", fixed = TRUE)
  expect_no_match(code, "outcome_means", fixed = TRUE)
})
