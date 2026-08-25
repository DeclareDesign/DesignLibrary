context("Simulate Designers")

test_that("step-level draws fan a designer's design out", {
  # DeclareDesign 2.0 fans out with `draws` on a step where 1.x took a vector
  # of sims. Two populations, and two runs of the estimator inside each.
  design <- two_arm_designer()
  attr(design[["model"]], "draws") <- 2L
  attr(design[["estimator"]], "draws") <- 2L
  sx <- simulate_design(design)
  expect_equal(sx$model_draw, c(1L, 1L, 2L, 2L))
  expect_equal(sx$estimator_draw, c(1L, 2L, 1L, 2L))
  # The estimator is deterministic given the data, so its two draws agree
  # within a population and differ across populations.
  expect_equal(sx$estimate[1], sx$estimate[2])
  expect_equal(sx$estimate[3], sx$estimate[4])
  expect_false(isTRUE(all.equal(sx$estimate[1], sx$estimate[3])))
})
