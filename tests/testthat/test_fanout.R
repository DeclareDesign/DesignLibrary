context("Fanout execution")

test_that("fan out IDs are correct", {

  sims <- c(30, 1, 2, 1, 1, 2)
  design <- simple_two_arm_designer(rho = 0)
  
  sx <- simulate_design(design, sims = sims)
  
  expect_equivalent(apply(sx[,c("step_1_draw","step_3_draw","step_6_draw")], 2, max), c(30, 60, 120))
})