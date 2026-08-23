---
id: two_arm_trial_rdss
alias: "18.1"
label: two-arm trial
category: rdss
keywords: [experiment, causal]
description: >
  Canonical two-arm trial design. (chapter 18).
params:
  "N": "Number of units (sample or population size)"
  "b": "Treatment effect (outcome scale)"
book_link: https://book.declaredesign.org/library/experimental-causal.html#def-ch18num1
include_in_shiny: false
---

design <-
  declare_parameters(
    N = 100,
    b = 0.2
  ) +
  declare_model(N = N,
                U = rnorm(N),
                potential_outcomes(Y ~ b * Z + U)) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N, prob = 0.5)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE")
