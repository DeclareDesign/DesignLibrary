---
id: two_arm_flexible
label: Flexible two-arm trial
category: template
keywords: [experiment, two-arm]
description: >
  One-level two-arm trial with complete random assignment. Potential
  outcomes are jointly normal, with correlation rho between the control
  and treatment shocks. Argument names match DesignLibrary::two_arm_designer;
  ate is the difference in means, so the treatment mean is control_mean + ate.
params:
  "N": "Sample size"
  "assignment_prob": "Probability of assignment to treatment"
  "control_mean": "Average outcome in control"
  "control_sd": "Standard deviation in control"
  "ate": "Average treatment effect"
  "treatment_sd": "Standard deviation in treatment"
  "rho": "Correlation between treatment and control potential outcomes, in [-1, 1]"
include_in_shiny: true
---

design <-
  declare_parameters(
    N = 100,
    assignment_prob = 0.5,
    control_mean = 0,
    control_sd = 1,
    ate = 1,
    treatment_sd = 1,
    rho = 1
  ) +
  declare_model(
    N = N,
    u_0 = rnorm(N),
    u_1 = rnorm(n = N, mean = rho * u_0, sd = sqrt(pmax(0, 1 - rho^2))),
    potential_outcomes(
      Y ~ control_mean +
          (1 - Z) * (u_0 * control_sd) + Z* (u_1 * treatment_sd + ate)
    )
  ) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N, prob = assignment_prob)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE")
