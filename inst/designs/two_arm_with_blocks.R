---
id: two_arm_with_blocks
label: Two-arm trial with blocks
category: template
keywords: [experiment, two-arm, blocking]
description: >
  Simple two arm design with block randomization
params:
  "b": "Treatment effect (outcome scale)"
  "block_size": "Units per block"
  "k": "Number of blocks"
include_in_shiny: true
---

design <-
  declare_parameters(
    k = 3,
    block_size = 10,
    b = .2
  ) +
  
  declare_model(
    blocks = add_level(N = k, u_block = rnorm(N)),
    i = add_level( N = block_size, u_i = rnorm(N),
      potential_outcomes(Y ~ b * Z + u_block + u_i)
  )) +
  
  declare_inquiry(ATE = mean(Y_Z_1) - mean(Y_Z_0)) +
  
  # blocking here:
  declare_assignment(Z = block_ra(blocks = blocks)) +
  
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  
  declare_estimator(
    Y ~ Z,
    .method = difference_in_means,
    blocks = blocks,
    inquiry = "ATE"
  )
