---
id: factorial_2x2x2
label: 2x2x2 factorial
category: template
keywords: [experiment, causal, factorial, interaction]
description: >
  Three-factor (2^3) factorial experiment with independent, equiprobable
  assignment to each factor. Inquiries are the average marginal effect of
  each factor, the three average two-way interactions, and the three-way
  interaction, each averaged over the conditions of the other factors.
  Estimation is a fully interacted regression of demeaned treatments, in
  which the intercept is the average outcome across all eight cells.
  Flattened from DesignLibrary::factorial_designer at k = 3.
params:
  "N": "Number of units"
  "effect_T1": "Effect of T1 when T2 and T3 are 0"
  "effect_T2": "Effect of T2 when T1 and T3 are 0"
  "effect_T3": "Effect of T3 when T1 and T2 are 0"
  "interaction_T1_T2": "Two-way interaction of T1 and T2 when T3 is 0"
  "interaction_T1_T3": "Two-way interaction of T1 and T3 when T2 is 0"
  "interaction_T2_T3": "Two-way interaction of T2 and T3 when T1 is 0"
  "interaction_T1_T2_T3": "Three-way interaction of T1, T2 and T3"
  "sd": "Standard deviation of the individual-level shock"
include_in_shiny: true
---

N <- 256
effect_T1 <- 0.5
effect_T2 <- 0
effect_T3 <- 0
interaction_T1_T2 <- 0
interaction_T1_T3 <- 0
interaction_T2_T3 <- 0
interaction_T1_T2_T3 <- 1
sd <- 1

design <-
  declare_model(
    N = N,
    u = rnorm(N) * sd,
    potential_outcomes(
      Y ~ effect_T1 * T1 + effect_T2 * T2 + effect_T3 * T3 +
        interaction_T1_T2 * T1 * T2 +
        interaction_T1_T3 * T1 * T3 +
        interaction_T2_T3 * T2 * T3 +
        interaction_T1_T2_T3 * T1 * T2 * T3 + u,
      conditions = list(T1 = c(0, 1), T2 = c(0, 1), T3 = c(0, 1))
    )
  ) +

  declare_inquiry(
    Overall_average = mean(c(
      Y_T1_0_T2_0_T3_0, Y_T1_1_T2_0_T3_0, Y_T1_0_T2_1_T3_0, Y_T1_1_T2_1_T3_0,
      Y_T1_0_T2_0_T3_1, Y_T1_1_T2_0_T3_1, Y_T1_0_T2_1_T3_1, Y_T1_1_T2_1_T3_1
    )),

    TE_T1 = mean(
      (Y_T1_1_T2_0_T3_0 - Y_T1_0_T2_0_T3_0) / 4 +
        (Y_T1_1_T2_1_T3_0 - Y_T1_0_T2_1_T3_0) / 4 +
        (Y_T1_1_T2_0_T3_1 - Y_T1_0_T2_0_T3_1) / 4 +
        (Y_T1_1_T2_1_T3_1 - Y_T1_0_T2_1_T3_1) / 4
    ),
    TE_T2 = mean(
      (Y_T1_0_T2_1_T3_0 - Y_T1_0_T2_0_T3_0) / 4 +
        (Y_T1_1_T2_1_T3_0 - Y_T1_1_T2_0_T3_0) / 4 +
        (Y_T1_0_T2_1_T3_1 - Y_T1_0_T2_0_T3_1) / 4 +
        (Y_T1_1_T2_1_T3_1 - Y_T1_1_T2_0_T3_1) / 4
    ),
    TE_T3 = mean(
      (Y_T1_0_T2_0_T3_1 - Y_T1_0_T2_0_T3_0) / 4 +
        (Y_T1_1_T2_0_T3_1 - Y_T1_1_T2_0_T3_0) / 4 +
        (Y_T1_0_T2_1_T3_1 - Y_T1_0_T2_1_T3_0) / 4 +
        (Y_T1_1_T2_1_T3_1 - Y_T1_1_T2_1_T3_0) / 4
    ),

    TE_T1_T2 = mean(
      ((Y_T1_1_T2_1_T3_0 - Y_T1_0_T2_1_T3_0) - (Y_T1_1_T2_0_T3_0 - Y_T1_0_T2_0_T3_0)) / 2 +
        ((Y_T1_1_T2_1_T3_1 - Y_T1_0_T2_1_T3_1) - (Y_T1_1_T2_0_T3_1 - Y_T1_0_T2_0_T3_1)) / 2
    ),
    TE_T1_T3 = mean(
      ((Y_T1_1_T2_0_T3_1 - Y_T1_0_T2_0_T3_1) - (Y_T1_1_T2_0_T3_0 - Y_T1_0_T2_0_T3_0)) / 2 +
        ((Y_T1_1_T2_1_T3_1 - Y_T1_0_T2_1_T3_1) - (Y_T1_1_T2_1_T3_0 - Y_T1_0_T2_1_T3_0)) / 2
    ),
    TE_T2_T3 = mean(
      ((Y_T1_0_T2_1_T3_1 - Y_T1_0_T2_0_T3_1) - (Y_T1_0_T2_1_T3_0 - Y_T1_0_T2_0_T3_0)) / 2 +
        ((Y_T1_1_T2_1_T3_1 - Y_T1_1_T2_0_T3_1) - (Y_T1_1_T2_1_T3_0 - Y_T1_1_T2_0_T3_0)) / 2
    ),

    TE_T1_T2_T3 = mean(
      ((Y_T1_1_T2_1_T3_1 - Y_T1_0_T2_1_T3_1) - (Y_T1_1_T2_0_T3_1 - Y_T1_0_T2_0_T3_1)) -
        ((Y_T1_1_T2_1_T3_0 - Y_T1_0_T2_1_T3_0) - (Y_T1_1_T2_0_T3_0 - Y_T1_0_T2_0_T3_0))
    )
  ) +

  declare_assignment(
    T1 = complete_ra(N),
    T2 = block_ra(blocks = T1),
    T3 = block_ra(blocks = paste(T1, T2))
  ) +

  declare_measurement(
    Y = reveal_outcomes(Y ~ T1 + T2 + T3),
    T1_c = T1 - 0.5,
    T2_c = T2 - 0.5,
    T3_c = T3 - 0.5
  ) +

  declare_estimator(
    Y ~ T1_c * T2_c * T3_c,
    term = c("(Intercept)", "T1_c", "T2_c", "T3_c",
             "T1_c:T2_c", "T1_c:T3_c", "T2_c:T3_c", "T1_c:T2_c:T3_c"),
    inquiry = c("Overall_average", "TE_T1", "TE_T2", "TE_T3",
                "TE_T1_T2", "TE_T1_T3", "TE_T2_T3", "TE_T1_T2_T3"),
    label = "demeaned interacted regression"
  )
