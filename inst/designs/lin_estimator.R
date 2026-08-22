---
id: lin_estimator
alias: "18.3"
label: lin estimator
category: rdss
keywords: [experiment, causal]
description: >
  Lin estimator design. (chapter 18).
params:
  "N": "Number of units (sample or population size)"
  "control_slope": "Slope of the control outcome surface on the covariate"
  "prob": "Probability of assignment to treatment"
book_link: https://book.declaredesign.org/library/experimental-causal.html#def-ch18num3
include_in_shiny: true
---

design <-
  declare_parameters(
    prob = 0.5,
    control_slope = -1,
    N = 100
  ) +
  declare_model(N = N,
                X = runif(N, 0, 1),
                U = rnorm(N, sd = 0.1),
                Y_Z_1 = 1*X + U,
                Y_Z_0 = control_slope*X + U
  ) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N = N, prob = prob)) + 
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE", label = "DIM") +
  declare_estimator(Y ~ Z + X, .method = lm_robust, inquiry = "ATE", label = "OLS") +
  declare_estimator(Y ~ Z, covariates = ~X, .method = lm_lin, inquiry = "ATE", label = "Lin")
