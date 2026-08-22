---
id: single_period_two_arm
alias: "18.11"
label: Comparison single-period two arm trial
category: rdss
keywords: [experiment, causal]
description: >
  Comparison single-period two arm trial design. (chapter 18).
params:
  "effect_size": "Treatment effect size"
  "n_units": "Number of units"
book_link: https://book.declaredesign.org/library/experimental-causal.html#def-ch18num11
include_in_shiny: false
---

design <-
  declare_parameters(
    n_units = 100,
    effect_size = .5
  ) +
  declare_model(
    N = n_units, 
    U_unit = rnorm(N),
    U = rnorm(N),
    effect_size = effect_size,
    potential_outcomes(Y ~ scale(U_unit + U) + effect_size * Z)
  ) +
  declare_assignment(Z = complete_ra(N, m = n_units / 2)) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) + 
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE", label = "DIM")
