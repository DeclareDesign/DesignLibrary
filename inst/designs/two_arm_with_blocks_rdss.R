---
id: two_arm_with_blocks_rdss
alias: "2.2"
label: Two-arm trial with blocks from RDSS
category: rdss
keywords: [experiment, two-arm, blocking, introduction]
description: >
  Chapter 2 design that uses background information (history) for blocked
  assignment and a blocked difference-in-means estimator.
params:
  "N": "Number of units (sample or population size)"
  "b": "Treatment effect (outcome scale)"
book_link: https://book.declaredesign.org/introduction/what-is-a-research-design.html#def-ch2num2
include_in_shiny: true
---

design <-
  declare_parameters(
    b = 0,
    N = 1000
  ) +
  
  declare_model(
    N = N,
    history = sample(c(0, 1), N, replace = TRUE),
    potential_outcomes(Y ~ b * history + runif(1, 0, 0.5) * Z + rnorm(N))
  ) +
  
  declare_inquiry(ATE = mean(Y_Z_1) - mean(Y_Z_0)) +
  
  declare_sampling(S = complete_rs(N = N, n = 100), filter = S == 1) +
  
  # blocking here:
  declare_assignment(Z = block_ra(blocks = history)) +
  
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  
  declare_estimator(
    Y ~ Z,
    .method = difference_in_means,
    blocks = history,
    inquiry = "ATE"
  )
