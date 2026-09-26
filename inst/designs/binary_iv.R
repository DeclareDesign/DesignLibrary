---
id: binary_iv
label: Binary instrumental variables
category: template
keywords: [experiment, causal, instrumental variables, noncompliance]
description: >
  Units are always-takers, never-takers, compliers, or defiers. The instrument
  Z can depend on type, the treatment X follows from type and Z, and the
  outcome can depend on type, X, and Z directly. The design compares OLS of Y
  on X and two-stage least squares against the average effect, the complier
  effect, and the effect identified when defiers exist.
  Flattened from DesignLibrary::binary_iv_designer.
params:
  "N": "Number of units"
  "type_probs": "Shares of always-takers, never-takers, compliers, and defiers"
  "assignment_probs": "Probability that Z = 1 for each of the four types"
  "outcome_sd": "Standard deviation of the outcome shock"
  "a": "Outcome intercept for each type"
  "b": "Effect of X on the outcome for each type"
  "d": "Direct effect of Z on the outcome for each type"
include_in_shiny: true
---

design <-
  declare_parameters(
    N = 100,
    type_probs = c(1/3, 1/3, 1/3, 0),
    assignment_probs = c(0.5, 0.5, 0.5, 0.5),
    outcome_sd = 1,
    a = c(1, 0, 0, 0),
    b = c(0, 0, 0, 0),
    d = c(0, 0, 0, 0)
  ) +
  declare_model(
    N = N,
    type = sample(1:4, N, replace = TRUE, prob = type_probs),
    type_label = c("Always", "Never", "Complier", "Defier")[type],
    u_Z = runif(N),
    u_Y = rnorm(N) * outcome_sd,
    Z = (u_Z < assignment_probs[type]),
    X = (type == 1) + (type == 3) * Z + (type == 4) * (1 - Z),
    potential_outcomes(Y ~ a[type] + b[type] * X + d[type] * Z + u_Y,
                       conditions = list(X = 0:1))
  ) +

  declare_measurement(Y = reveal_outcomes(Y ~ X)) +

  declare_inquiry(
    first_stage = mean((type == 3) - (type == 4)),
    ate = mean(Y_X_1 - Y_X_0),
    late = mean(Y_X_1[type == 3] - Y_X_0[type == 3]),
    late_het = (mean(type == 3) * mean(Y_X_1[type == 3] - Y_X_0[type == 3]) -
                  mean(type == 4) * mean(Y_X_1[type == 4] - Y_X_0[type == 4])) /
      (mean(type == 3) - mean(type == 4))
  ) +

  declare_estimator(X ~ Z, .method = difference_in_means,
                    inquiry = "first_stage", label = "d-i-m") +
  declare_estimator(Y ~ X, .method = lm_robust,
                    inquiry = c("ate", "late", "late_het"), label = "lm_robust") +
  declare_estimator(Y ~ X | Z, .method = iv_robust,
                    inquiry = c("ate", "late", "late_het"), label = "iv_robust")
