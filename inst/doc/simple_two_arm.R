## ----MIDA, echo = FALSE,include = FALSE----------------------------------
library(DesignLibrary)

## ---- code = designer_default_args_text(simple_two_arm_designer)---------
N <- 10
prob <- 0.5
control_mean <- 0
control_sd <- 1
ate <- 0
treatment_mean <- control_mean + ate
treatment_sd <- control_sd
rho <- 1

## ---- code = simple_two_arm_designer(code = TRUE)------------------------
# M: Model
pop <- declare_population(
  N = N,
  Z0 = rnorm(N, mean = control_mean, sd = control_sd),
  Z1 = correlate(given = Z0, rho = rho, rnorm, mean = treatment_mean, sd = treatment_sd)
)
potential_outcomes <- declare_potential_outcomes(Y ~ (1-Z) * Z0 + Z * Z1)

# I: Inquiry
estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))

# D: Data
assignment <- declare_assignment(prob = prob)

# A: Analysis
estimator <- declare_estimator(Y ~ Z, estimand = estimand)

# Design
simple_two_arm_design <- pop / potential_outcomes / estimand /assignment / declare_reveal() /estimator

## ----simple_two_arm_diagnosis,echo = FALSE-------------------------------
diagnosis <- get_or_run_diagnosis(simple_two_arm_design, sims = 1000, bootstrap = FALSE)
knitr::kable(diagnosis$diagnosands,digits = 2)

## ----simple_two_arm_shiny_diagnosis,include = FALSE----------------------
get_or_run_shiny_diagnosis(simple_two_arm_designer, sims = 1000, bootstrap = FALSE)

## ----eval = FALSE--------------------------------------------------------
#  diagnosis <- diagnose_design(simple_two_arm_design, sims = 1000, bootstrap = FALSE)

