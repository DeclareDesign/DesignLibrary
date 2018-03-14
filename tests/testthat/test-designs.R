context("Test Designs")

test_that("Mediation works", {
  
  vary_N_mediation <- fill_out(template = mediation_template, rho = seq(-1,1,.2))
  
  diagnosis <- DeclareDesign::diagnose_design(vary_N_mediation,sims = 10,bootstrap = F)
  
  diagnosis

  })


test_that("Two-Way Factorial works", {
  
  two_way <- two_way_factorial_template()
  
  DeclareDesign::diagnose_design(two_way, sims = 10,bootstrap = F)
  
  vary_N_2way <- fill_out(template = two_way_factorial_template, N = c(100,200,300))
  
  DeclareDesign::diagnose_design(vary_N_2way, sims = 10,bootstrap = F)
  
})

test_that("Regression Discontinuity works", {
  
  
  RD <- regression_discontinuity_template()
  
  DeclareDesign::diagnose_design(RD, sims = 100,bootstrap = F)
  
  vary_N_RD <-
    fill_out(template = regression_discontinuity_template, N = c(100, 200, 300))
  
  DeclareDesign::diagnose_design(vary_N_RD, sims = 100, bootstrap = F)
  
})

test_that("Two Arm works", {
  
  two_arm <- two_arm_template()
  
  DeclareDesign::diagnose_design(two_arm, sims = 100,bootstrap = F)
  
  vary_N_two_arm <-
    fill_out(template = two_arm_template, N = c(200, 300, 400))
  
  DeclareDesign::diagnose_design(vary_N_two_arm, sims = 100, bootstrap = F)
  
})



test_that("Noncompliance works", {
  
  noncompliance <- noncompliance_template()
  DeclareDesign::diagnose_design(noncompliance, sims = 10, bootstrap = F)
  
  vary_N_noncompliance <- fill_out(template = noncompliance_template, N = c(200, 300, 400))
  DeclareDesign::diagnose_design(vary_N_noncompliance, sims = 10, bootstrap = F)
  
})


