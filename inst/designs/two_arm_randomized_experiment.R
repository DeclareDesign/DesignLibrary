---
id: two_arm_randomized_experiment
alias: "13.2"
label: Declaration of two-arm randomized experiment
category: rdss
description: >
  Declaration of two-arm randomized experiment (chapter 13).
params:
  "N": "Number of units (sample or population size)"
book_link: https://book.declaredesign.org/declaration-diagnosis-redesign/declaration-in-code.html#def-ch13num2
include_in_shiny: false
---

design <-
  declare_parameters(
    N = 1000
  ) +
  declare_model(N = N,
                U = rnorm(N),
                X = U + rnorm(N, sd = 0.5),
                potential_outcomes(Y ~  0.2 * Z + U)) +
  
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +

  declare_sampling(S = simple_rs(N, prob = 0.2), 
                   filter = S == 1) + 
  declare_assignment(Z = complete_ra(N)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE", label = "DIM") +
  declare_estimator(Y ~ Z + X, inquiry = "ATE", label = "OLS")
