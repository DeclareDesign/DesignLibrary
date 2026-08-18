---
id: multi_arm_three
label: three-arm trial
category: template
keywords: [experiment, causal, multiarm]
description: >
  Three-arm experiment with equal assignment probabilities. Inquiries are
  the three pairwise average treatment effects and estimation is a
  difference in means within each pair of arms.
  Flattened from DesignLibrary::multi_arm_designer at m_arms = 3.
params:
  "N": "Number of units"
  "mean_1": "Average outcome in arm 1"
  "mean_2": "Average outcome in arm 2"
  "mean_3": "Average outcome in arm 3"
  "sd_i": "Standard deviation of the individual-level shock"
include_in_shiny: true
---

N <- 90
mean_1 <- 0
mean_2 <- 0.5
mean_3 <- 2
sd_i <- 1

design <-
  declare_model(
    N = N,
    u = rnorm(N) * sd_i,
    potential_outcomes(
      Y ~ mean_1 * (Z == "1") + mean_2 * (Z == "2") + mean_3 * (Z == "3") + u,
      conditions = list(Z = c("1", "2", "3"))
    )
  ) +

  declare_inquiry(
    ate_Y_2_1 = mean(Y_Z_2 - Y_Z_1),
    ate_Y_3_1 = mean(Y_Z_3 - Y_Z_1),
    ate_Y_3_2 = mean(Y_Z_3 - Y_Z_2)
  ) +

  declare_assignment(Z = complete_ra(N, conditions = c("1", "2", "3"))) +

  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +

  declare_estimator(Y ~ Z, .method = difference_in_means,
                    condition1 = "1", condition2 = "2",
                    inquiry = "ate_Y_2_1", label = "DIM (Z_2 - Z_1)") +
  declare_estimator(Y ~ Z, .method = difference_in_means,
                    condition1 = "1", condition2 = "3",
                    inquiry = "ate_Y_3_1", label = "DIM (Z_3 - Z_1)") +
  declare_estimator(Y ~ Z, .method = difference_in_means,
                    condition1 = "2", condition2 = "3",
                    inquiry = "ate_Y_3_2", label = "DIM (Z_3 - Z_2)")
