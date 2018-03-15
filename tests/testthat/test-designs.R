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

test_that("Randomized Response works", {
  
  randomized_response <- randomized_response_template()
  
  DeclareDesign::diagnose_design(randomized_response, sims = 100,bootstrap = F)
  
  vary_N_randomized_response <-
    fill_out(template = randomized_response_template, N = c(200, 300, 400))
  
  DeclareDesign::diagnose_design(vary_N_randomized_response, sims = 100, bootstrap = F)
  
})
  
test_that("Noncompliance works", {
  
  noncompliance <- noncompliance_template()
  DeclareDesign::diagnose_design(noncompliance, sims = 10, bootstrap = F)
  
  vary_N_noncompliance <- fill_out(template = noncompliance_template, N = c(200, 300, 400))
  DeclareDesign::diagnose_design(vary_N_noncompliance, sims = 10, bootstrap = F)
  
})

test_that("List Experiment works", {
  
  list_experiment <- list_experiment_template()
  diagnose_design(list_experiment, sims = 100, bootstrap = F)
  
  vary_N_list_experiment <- fill_out(template = list_experiment_template, N = c(200, 300, 400))
  DeclareDesign::diagnose_design(vary_N_list_experiment, sims = 10, bootstrap = F)
  
})

test_that("Matching works", {
  
  matching_diagnosands <-
    declare_diagnosands(bias = mean(estimand - est),
                        rmse = mean((estimand - est) ^ 2))
  
  match_out <- matching_template()
  diagnose_design(match_out, sims = 100, bootstrap = F,
                  diagnosands = matching_diagnosands)
  
  vary_N_matching <- fill_out(template = matching_template, N = c(200, 300, 400))
  DeclareDesign::diagnose_design(vary_N_matching, sims = 10, bootstrap = F, diagnosands = matching_diagnosands)
  
  
})


test_that("Process Tracing works",{
  
  process_tracing_diagnosands <- declare_diagnosands(
    truth = mean(estimand),
    mean_guess = mean(guess),
    bias = mean(guess - estimand),
    bias_given_K_seen = mean(guess[K_seen] - estimand[K_seen]),
    bias_given_no_K_seen = mean(guess[!K_seen] - estimand[!K_seen]))
  
  process_tracing <- process_tracing_template()
  
  DeclareDesign::diagnose_design(process_tracing, sims = 100,bootstrap = F,diagnosands = process_tracing_diagnosands)
  
  vary_N_process_tracing <-
    fill_out(template = process_tracing_template, N = c(200, 300, 400))
  
  DeclareDesign::diagnose_design(vary_N_process_tracing, sims = 100, 
                                 bootstrap = F,
                                 diagnosands = process_tracing_diagnosands)
})

test_that("Election Forecast works", {
  
  election_forecast <- election_forecast_template()
  diagnose_design(election_forecast, sims = 10, bootstrap = F)
  
  vary_N_election_forecast <- fill_out(template = election_forecast_template, N = c(200, 300, 400))
  DeclareDesign::diagnose_design(vary_N_election_forecast, sims = 10, bootstrap = F)
  
})

test_that("SRS works", {
  
  srs <- srs_template()
  diagnose_design(srs, sims = 10, bootstrap = F)
  
  vary_N_srs <- fill_out(template = srs_template, n = c(200, 300, 400))
  DeclareDesign::diagnose_design(vary_N_srs, sims = 10, bootstrap = F)
  
})

test_that("Audit works", {
  
  audit <- audit_template()
  diagnose_design(audit, sims = 10, bootstrap = F)
  
  vary_N_audit <-
    fill_out(template = audit_template, N = c(200, 300, 400))
  DeclareDesign::diagnose_design(vary_N_audit, sims = 10, bootstrap = F)
  
})

