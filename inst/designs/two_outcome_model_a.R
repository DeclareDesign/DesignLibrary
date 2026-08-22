---
id: two_outcome_model_a
alias: "10.3a"
label: Two-outcome model A
category: rdss
description: >
  10.3 (chapter 10).
params:
  "n": "Number of units (sample or population size)"
include_in_shiny: false
---

design <-
  declare_parameters(
    n = 150
  ) +
  declare_model(
    N = n,
    U = rnorm(N),
    potential_outcomes(Y1 ~ 0.2 * Z + U),
    potential_outcomes(Y2 ~ 0.0 * Z + U)
  ) +

  declare_inquiry(ATE1 = mean(Y1_Z_1 - Y1_Z_0),
                  ATE2 = mean(Y2_Z_1 - Y2_Z_0)) +
  declare_assignment(Z = complete_ra(N)) +
  declare_measurement(Y1 = reveal_outcomes(Y1 ~ Z),
                      Y2 = reveal_outcomes(Y2 ~ Z)) +
  declare_estimator(Y1 ~ Z, inquiry = "ATE1", label = "DIM1") +
  declare_estimator(Y2 ~ Z, inquiry = "ATE2", label = "DIM2")
