---
id: two_arm_trial
label: Simple two-arm trial
category: template
keywords: [experiment, two-arm]
description: >
  Simple two-arm trial with complete random assignment.
params:
  "N": "Number of units (sample or population size)"
  "b": "Treatment effect (outcome scale)"
diagnosands: [bias, power]
include_in_shiny: true
---

design <-
  declare_parameters(
    N = 1000,
    b = 0.2
  ) +
  
  declare_model(
    N = N,
    potential_outcomes(Y ~ b * Z + rnorm(N))
  ) +

  declare_inquiry(ATE = mean(Y_Z_1) - mean(Y_Z_0)) +
  
  declare_assignment(Z = complete_ra(N)) +
  
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  
  declare_estimator(Y ~ Z, .method = difference_in_means, inquiry = "ATE")
