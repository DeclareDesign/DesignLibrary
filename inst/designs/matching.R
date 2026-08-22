---
id: matching
alias: "16.2"
label: matching
category: rdss
keywords: [observational, causal]
description: >
  Matching design declaration. (chapter 16).
packages: [MatchIt]
params:
  "N": "Number of units (sample or population size)"
  "ate": "Average treatment effect of D on Y"
  "exact_matching": "Function: exact-match treated to controls on covariate values (R-only)"
book_link: https://book.declaredesign.org/library/observational-causal.html#def-ch16num2
include_in_shiny: true
---


exact_matching <- 
  function(data) { 
    matched <- matchit(D ~ X, method = "exact", data = data) 
    match.data(matched) 
  }

design <-
  declare_parameters(
    N = 100,
    ate = 0.5
  ) +
  declare_model(
    N = N, 
    U = rnorm(N), 
    X = rbinom(N, 1, prob = 0.5),
    D = rbinom(N, 1, prob = 0.25 + 0.5 * X),
    Y_D_0 = 0.2 * X + U,
    Y_D_1 = Y_D_0 + ate
  ) + 
  declare_inquiry(ATE = mean(Y_D_1 - Y_D_0)) +
  declare_step(handler = exact_matching) +
  declare_measurement(Y = reveal_outcomes(Y ~ D)) +
  declare_estimator(Y ~ D,
                    weights = weights,
                    .method = difference_in_means,
                    label = "Matched difference-in-means") +
  declare_estimator(Y ~ D, 
                    .method = difference_in_means, 
                    label = "Raw difference-in-means")
