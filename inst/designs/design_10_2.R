---
id: design_10_2
alias: "10.2"
label: design 10 2
category: rdss
description: >
  10.2 (chapter 10).
params:
  "N": "Number of units (sample or population size)"
include_in_shiny: false
---

design <-
  declare_parameters(
    N = 200
  ) +
  declare_model(
    N = N, U = rnorm(N),
    # this runif(n = 1, min = 0, max = 0.5) 
    # generates 1 random ATE between 0 and 0.5
    potential_outcomes(Y ~ runif(n = 1, min = 0, max = 0.5) * Z + U)) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N, prob = 0.5)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE")
