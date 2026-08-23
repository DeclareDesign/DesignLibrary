---
id: two_arm_simple
label: Simple two-arm trial
alias: "18.1"
category: template
keywords: [experiment, two-arm]
description: >
  Simple two-arm trial with complete random assignment and a population average treatment effect.
params:
  "N": "Number of units (sample or population size)"
  "b": "Treatment effect (outcome scale)"
diagnosands: [bias, power]
include_in_shiny: true
---

design <-
  declare_parameters(
    N = 1000,  # Number of units
    b = 0.2    # ATE
  ) +
  
  declare_model(N = N, Y0 = rnorm(n()), Y1  = b + rnorm(n())) +

  declare_inquiry(ATE = b) +
  
  declare_assignment(Z = complete_ra(n())) +
  
  declare_measurement(Y = Z*Y1 + (1-Z)*Y0) +
  
  declare_estimator(Y ~ Z)
