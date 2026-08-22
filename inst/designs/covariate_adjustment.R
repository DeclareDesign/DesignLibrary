---
id: covariate_adjustment
alias: "18.2"
label: covariate adjustment
category: rdss
keywords: [experiment, causal]
description: >
  Two-arm trial with covariate adjustment. (chapter 18).
params:
  "N": "Number of units (sample or population size)"
  "prob": "Probability of assignment to treatment"
  "r_sq": "R-squared of covariates with potential outcomes"
book_link: https://book.declaredesign.org/library/experimental-causal.html#def-ch18num2
include_in_shiny: true
---


# We have three estimators: Lin is the one to use
# The others are for comparison purposes

design <-
  declare_parameters(
    N = 100,
    r_sq = 0,
    prob = 0.7
  ) +
  declare_model(N = N,
                draw_multivariate(c(U, X) ~ MASS::mvrnorm(
                  n = N,
                  mu = c(0, 0),
                  Sigma = matrix(c(1, sqrt(r_sq), sqrt(r_sq), 1), 2, 2)
                )), 
                potential_outcomes(Y ~ 0.1 * Z + U)) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N, prob = prob)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, covariates = ~X, .method = lm_lin, label = "Lin") +
  declare_estimator(Y ~ Z, label = "No controls")  +
  declare_estimator(Y ~ Z + X, label = "Linear OLS")
