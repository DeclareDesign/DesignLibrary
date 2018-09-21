context("Fanout execution")

test_that("fan out IDs are correct", {
  
  skip_if_not_installed("DesignLibrary")
  
  sims <- c(30, 1, 2, 1, 1, 2)
  design <- DesignLibrary::simple_two_arm_designer(rho = 0)
  
  sx <- simulate_design(design, sims = sims)
  
  expect_equivalent(apply(sx[,c(15, 16, 17)], 2, max), c(30, 60, 120))
})