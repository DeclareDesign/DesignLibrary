# DeclareDesign workflows run on designer output: simulation fan-out,
# redesign(), and diagnosing several designs at once.

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

test_that("fan-out IDs count every combination of step draws", {
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

test_that("redesign() reaches an argument the assignment step reads", {
  design_1 <- two_arm_designer(assignment_prob = 0)
  design_2 <- redesign(design_1, assignment_prob = 1)
  expect_true(all(draw_data(design_1)$Z == 0))
  expect_true(all(draw_data(design_2)$Z == 1))
})

test_that("redesign() over a vector of values returns one design per value", {
  designs <- redesign(two_arm_designer(), N = c(50, 100))
  expect_length(designs, 2)
  expect_equal(vapply(designs, function(d) nrow(draw_data(d)), integer(1)), c(50L, 100L),
               ignore_attr = TRUE)
})

test_that("designs from different designers diagnose together", {
  design_1 <- two_by_two_designer(N = 500, outcome_means = c(0, 0, 1, 2), weight_A = 0, weight_B = 0)
  design_2 <- multi_arm_designer(N = 500, m_arms = 3, outcome_means = c(0, 0, 1))
  dx <- diagnose_design(design_1, design_2, sims = 3, bootstrap_sims = FALSE)
  expect_setequal(unique(dx$diagnosands_df$design), c("design_1", "design_2"))
  expect_s3_class(reshape_diagnosis(dx), "data.frame")
})
