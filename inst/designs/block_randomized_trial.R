---
id: two_arm_block_rdss
alias: "18.4"
label: block randomized trial
category: rdss
keywords: [experiment, causal, blocking]
description: >
  Block randomized two-arm trial design. (chapter 18).
params:
  "N": "Number of units (sample or population size)"
  "b": "Treatment effect (outcome scale)"
book_link: https://book.declaredesign.org/library/experimental-causal.html#def-ch18num4
include_in_shiny: true
---

design <-
  declare_parameters(
    b = 0.2,
    N = 500
  ) +
  declare_model(
    N = N,
    X = rep(c(0, 1), each = N / 2),
    U = rnorm(N, sd = 0.25),
    potential_outcomes(Y ~ b * Z + X + U)
  ) +
  declare_inquiry(ATE = b) +
  declare_assignment(
    Z = block_ra(blocks = X, block_prob = c(0.2, 0.5)),
    probs =
      obtain_condition_probabilities(assignment = Z, 
                                     blocks = X, 
                                     block_prob = c(0.2, 0.5)),
    ipw = 1 / probs
  ) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(
    Y ~ Z,
    covariates = ~ X,
    .method = lm_lin,
    weights = ipw,
    label = "Lin"
  ) +
  declare_estimator(
    Y ~ Z,
    .method = difference_in_means,
    blocks = X,
    label = "Block differences in means"
  )
