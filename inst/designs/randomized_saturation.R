---
id: randomized_saturation
alias: "18.12"
label: randomized saturation
category: rdss
keywords: [experiment, causal]
description: >
  Randomized saturation design. (chapter 18).
packages: [dplyr]
params:
  "N_groups": "Number of groups / clusters"
  "N_per_group": "Individuals per group"
  "effect_Z": "Main effect of individual treatment Z"
  "effect_S_low": "Additive effect of low saturation S"
  "effect_S_high": "Additive effect of high saturation S"
book_link: https://book.declaredesign.org/library/experimental-causal.html#def-ch18num12
include_in_shiny: true
---

design <-
  declare_parameters(
    N_groups = 50,
    N_per_group = 20,
    effect_Z = 0.2,
    effect_S_low = 0.1,
    effect_S_high = 0.5
  ) +
  declare_model(
    group = add_level(N = N_groups, group_shock = rnorm(N)),
    individual = add_level(
      N = N_per_group,
      individual_shock = rnorm(N),
      potential_outcomes(
        Y ~ effect_Z * Z + effect_S_low * (S == "low") + effect_S_high * (S == "high") +
          group_shock + individual_shock,
        conditions = list(Z = c(0, 1),
                          S = c("low", "high"))
      )
    )
  ) +
  declare_inquiry(
    CATE_S_Z_0 = mean(Y_Z_0_S_high - Y_Z_0_S_low),
    CATE_Z_S_low = mean(Y_Z_1_S_low - Y_Z_0_S_low)
  ) +
  declare_assignment(
    S = cluster_ra(clusters = group,
                   conditions = c("low", "high")),
    Z = block_ra(blocks = group,
                 prob_unit = if_else(S == "low", 0.25, 0.75))
  ) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z + S)) +
  declare_estimator(
    Y ~ S,
    .method = difference_in_means,
    subset = Z == 0,
    term = "Shigh",
    clusters = group,
    inquiry = "CATE_S_Z_0",
    label = "Effect of high saturation among untreated"
  ) +
  declare_estimator(
    Y ~ Z,
    .method = difference_in_means,
    subset = S == "low",
    blocks = group,
    inquiry = "CATE_Z_S_low",
    label = "Effect of treatment at low saturation"
  )
