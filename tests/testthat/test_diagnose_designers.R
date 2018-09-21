context("Diagnose Designers")

test_that("with and without term",{
  design_1 <- simple_factorial_designer(N = 500, outcome_means = c(0,0,1,2), w_A = 0, w_B = 0)
  design_2 <- multi_arm_designer(N = 500, m_arms = 3, outcome_means = c(0, 0, 1))
  dx <- diagnose_design(design_1, design_2, sims = 3, bootstrap_sims = FALSE)
  
  expect_true(all(c("design_1", "design_2") %in% dx$diagnosands_df$design_label))
  
})