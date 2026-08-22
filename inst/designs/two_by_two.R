---
id: two_by_two
label: 2x2 factorial (library)
category: template
keywords: [experiment, causal, factorial]
description: >
  Two-by-two factorial with independent assignment of A then B within
  blocks of A. Cell means are the vector outcome_means in order AB =
  00, 01, 10, 11. Inquiries are the weighted average effect of each
  factor and the interaction. Argument names match
  DesignLibrary::two_by_two_designer.
params:
  "N": "Sample size"
  "prob_A": "Probability of assignment to A = 1"
  "prob_B": "Probability of assignment to B = 1"
  "weight_A": "Weight on A = 1 when defining the average effect of B"
  "weight_B": "Weight on B = 1 when defining the average effect of A"
  "outcome_means": "Average outcome in cells AB = 00, 01, 10, 11"
  "sd_i": "Standard deviation of the individual-level shock"
  "outcome_sds": "Extra standard deviation in each cell AB = 00, 01, 10, 11"
include_in_shiny: true
---

design <-
  declare_parameters(
    N = 100,
    prob_A = 0.5,
    prob_B = 0.5,
    weight_A = 0.5,
    weight_B = 0.5,
    outcome_means = c(0, 0, 0, 0),
    sd_i = 1,
    outcome_sds = c(0, 0, 0, 0)
  ) +
  declare_model(
    N = N,
    u = rnorm(N, sd = sd_i),
    potential_outcomes(
      Y ~ outcome_means[1 + B + 2 * A] + u +
        rnorm(N, sd = outcome_sds[1 + B + 2 * A]),
      conditions = list(A = c(0, 1), B = c(0, 1))
    )
  ) +
  declare_inquiry(
    ate_A = weight_B * mean(Y_A_1_B_1 - Y_A_0_B_1) +
      (1 - weight_B) * mean(Y_A_1_B_0 - Y_A_0_B_0),
    ate_B = weight_A * mean(Y_A_1_B_1 - Y_A_1_B_0) +
      (1 - weight_A) * mean(Y_A_0_B_1 - Y_A_0_B_0),
    interaction = mean((Y_A_1_B_1 - Y_A_1_B_0) - (Y_A_0_B_1 - Y_A_0_B_0))
  ) +
  declare_assignment(
    A = complete_ra(N, prob = prob_A),
    B = block_ra(blocks = A, prob = prob_B)
  ) +
  declare_measurement(Y = reveal_outcomes(Y ~ A + B)) +
  declare_estimator(
    Y ~ A + B, .method = lm_robust, term = c("A", "B"),
    inquiry = c("ate_A", "ate_B"), label = "No_Interaction"
  ) +
  declare_estimator(
    Y ~ A + B + A:B, .method = lm_robust, term = "A:B",
    inquiry = "interaction", label = "Interaction"
  )
