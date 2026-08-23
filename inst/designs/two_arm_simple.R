---
id: two_arm_simple
label: Simple two-arm trial
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
  
  # M: model
  declare_model(N = N, Y_Z_0 = rnorm(n()), Y_Z_1  = Y0 + b) +
  
  # I: Inquiry
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +

  # D: Data strategy  
  declare_assignment(Z = complete_ra(n())) +

  declare_measurement(Y = Z*Y_Z_1 + (1-Z)*Y_Z_0) +
  
  # A: Answer strategy
  declare_estimator(Y ~ Z, .method = difference_in_means, inquiry = "ATE")