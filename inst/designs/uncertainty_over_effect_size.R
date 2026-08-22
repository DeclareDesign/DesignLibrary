---
id: uncertainty_over_effect_size
alias: "11.2"
label: uncertainty over effect size
category: rdss
description: >
  Uncertainty over effect size design. (chapter 11).
params:
  "N": "Number of units (sample or population size)"
book_link: https://book.declaredesign.org/declaration-diagnosis-redesign/redesigning.html#def-ch11num2
include_in_shiny: true
---

design <-
  declare_parameters(
    N = 100
  ) +
  declare_model(N = N, U = rnorm(N),
                # this runif(n = 1, min = 0, max = 0.5) generates 1 random ATE between 0 and 0.5
                potential_outcomes(Y ~ runif(n = 1, min = 0, max = 0.5) * Z + U)) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N, prob = 0.5)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE")
