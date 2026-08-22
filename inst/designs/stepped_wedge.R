---
id: stepped_wedge
alias: "18.10"
label: stepped-wedge
category: rdss
keywords: [experiment, causal]
description: >
  Stepped-wedge design. (chapter 18).
packages: [dplyr]
params:
  "effect_size": "Treatment effect size"
book_link: https://book.declaredesign.org/library/experimental-causal.html#def-ch18num10
include_in_shiny: true
---

design <-
  declare_parameters(
    effect_size = 0.35
  ) +
  declare_model(
    units = declare_level(
      N = 100, 
      U_unit = rnorm(N)
    ),
    periods = declare_level(
      N = 3,
      time = 1:max(periods),
      U_time = rnorm(N)
    ),
    unit_period = cross_levels(
      .by = c("units", "periods"),
      U = rnorm(N),
      potential_outcomes(
        Y ~ scale(U_unit + U_time + time + U) + effect_size * Z
      )
    )
  ) +
  declare_assignment(
    wave = cluster_ra(clusters = units, conditions = 1:max(periods)),
    Z = if_else(time >= wave, 1, 0)
  ) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0), subset = time < max(time)) + 
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, fixed_effects = ~ periods + units, 
                    clusters = units, 
                    subset = time < max(time),
                    inquiry = "ATE", label = "TWFE")
