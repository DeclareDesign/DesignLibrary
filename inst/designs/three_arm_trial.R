---
id: three_arm
label: Three-arm trial
category: template
keywords: [experiment, causal, three-arm]
description: >
  Three-arm experiment with modifiable assignment probabilities. 
params:
  "N": "Number of units"
  "sd_i": "Standard deviation of fixed individual-level shock"
  "outcome_means": "Average outcome in each arm"
  "prob_each": "Assignment probabilities"
include_in_shiny: true
---

design <-
  declare_parameters(
    N = 90,
    outcome_means = c(0, .1, .2),
    sd_i = 1,
    prob_each = c(1,1,1)/3
  ) +
  declare_model(
    N = N,
    u_i = rnorm(N) * sd_i,
  ) +
  declare_inquiry(
    ate_2 = as.numeric(outcome_means[2] - outcome_means[1]),
    ate_3 = as.numeric(outcome_means[3] - outcome_means[1])
  ) +
  declare_assignment(
    Z = complete_ra(N, conditions = 1:3, prob_each = prob_each)
  ) +
  declare_measurement(
    Y = outcome_means[Z] + u_i,
    Z2 = 1*(Z==2),
    Z3 = 1*(Z==3)) +
  declare_estimator(
    Y ~ Z2 + Z3,
    term = c("Z2", "Z3"),
    inquiry = c("ate_2", "ate_3")
  )
