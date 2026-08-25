context("Fanout execution")

test_that("fan out IDs are correct", {
  design <- two_arm_designer(rho = 0)
  attr(design[["model"]], "draws") <- 5L
  attr(design[["assignment"]], "draws") <- 2L
  attr(design[["estimator"]], "draws") <- 2L
  sx <- simulate_design(design)
  expect_equal(nrow(sx), 20L)
  expect_equal(max(sx$model_draw), 5L)
  expect_equal(max(sx$assignment_draw), 2L)
  expect_equal(max(sx$estimator_draw), 2L)
  expect_equal(max(sx$sim_ID), 20L)
})
