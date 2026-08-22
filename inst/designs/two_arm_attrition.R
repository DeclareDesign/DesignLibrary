---
id: two_arm_attrition
label: Two-arm trial with attrition
category: template
keywords: [experiment, causal, attrition]
description: >
  Two-arm trial in which the outcome is observed only for units that report.
  Treatment can move reporting as well as the outcome, so the design contrasts
  the effect on reporting, the effect on the outcome among reporters, and the
  effect on the outcome had it been observed for everyone.
  Flattened from DesignLibrary::two_arm_attrition_designer.
params:
  "N": "Number of units"
  "a_R": "Constant in the equation relating treatment to reporting"
  "b_R": "How reporting is related to treatment"
  "a_Y": "Constant in the equation relating treatment to the outcome"
  "b_Y": "Slope relating treatment to the outcome"
  "rho": "Correlation between the reporting and outcome shocks, in [0, 1]"
include_in_shiny: true
---

design <-
  declare_parameters(
    N = 100,
    a_R = 0,
    b_R = 1,
    a_Y = 0,
    b_Y = 1,
    rho = 0
  ) +
  declare_model(
    N = N,
    u_R = rnorm(N),
    u_Y = rnorm(N, mean = rho * u_R, sd = sqrt(1 - rho^2)),
    potential_outcomes(R ~ (a_R + b_R * Z > u_R)),
    potential_outcomes(Y ~ (a_Y + b_Y * Z > u_Y))
  ) +

  declare_assignment(Z = complete_ra(N, prob = 0.5)) +

  declare_measurement(
    R = reveal_outcomes(R ~ Z),
    Y = reveal_outcomes(Y ~ Z),
    Y_obs = ifelse(R, Y, NA)
  ) +

  declare_inquiry(
    `ATE on R` = mean(R_Z_1 - R_Z_0),
    `ATE on Y` = mean(Y_Z_1 - Y_Z_0),
    `ATE on Y (Given R)` = mean((Y_Z_1 - Y_Z_0)[R == 1])
  ) +

  declare_estimator(R ~ Z, term = "Z", inquiry = "ATE on R",
                    label = "DIM on R") +
  declare_estimator(Y_obs ~ Z, term = "Z",
                    inquiry = c("ATE on Y", "ATE on Y (Given R)"),
                    label = "DIM on Y_obs") +
  declare_estimator(Y ~ Z, term = "Z",
                    inquiry = c("ATE on Y", "ATE on Y (Given R)"),
                    label = "DIM on Y")
