context("Simulate Designers")

test_that("MH sim ids", {
  design <- two_arm_designer()
  sx <- simulate_design(design, sims = c(2, 1, 1, 1, 1, 2))
  expect_equal(sx$step_1_draw, c(1L, 1L, 2L, 2L))
  expect_equal(sx$step_6_draw, c(1L, 2L, 3L, 4L))
  expect_equal(sx$estimate[1], sx$estimate[2])
  expect_equal(sx$estimate[3], sx$estimate[4])
})