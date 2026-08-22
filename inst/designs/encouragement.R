---
id: encouragement
alias: "18.8"
label: encouragement
category: rdss
keywords: [experiment, causal]
description: >
  Encouragement design. (chapter 18).
packages: [dplyr]
params:
  "N": "Number of units (sample or population size)"
  "share_always_takers": "Share of always-takers"
  "share_compliers": "Share of compliers"
  "share_defiers": "Share of defiers"
  "share_never_takers": "Share of never-takers"
book_link: https://book.declaredesign.org/library/experimental-causal.html#def-ch18num8
include_in_shiny: true
---

# sample size

# shares of population of different response types (add to 1!)

design <-
  declare_parameters(
    N = 100,
    share_always_takers = .2,
    share_never_takers = .2,
    share_compliers = .6,
    share_defiers = 0
  ) +
  declare_model(
    N = N,
    type = 
      rep(c("Always-Taker", "Never-Taker", "Complier", "Defier"),
          c(share_always_takers, share_never_takers, share_compliers, share_defiers)*N),
    U = rnorm(N),
    # potential outcomes of Y with respect to D
    potential_outcomes(
      Y ~ case_when(
        type == "Always-Taker" ~ -0.25 - 0.50 * D + U,
        type == "Never-Taker" ~ 0.75 - 0.25 * D + U,
        type == "Complier" ~ 0.25 + 0.50 * D + U,
        type == "Defier" ~ -0.25 - 0.50 * D + U
      ),
      conditions = list(D = c(0, 1))
    ),
    # potential outcomes of D with respect to Z
    potential_outcomes(
      D ~ case_when(
        Z == 1 & type %in% c("Always-Taker", "Complier") ~ 1,
        Z == 1 & type %in% c("Never-Taker", "Defier") ~ 0,
        Z == 0 & type %in% c("Never-Taker", "Complier") ~ 0,
        Z == 0 & type %in% c("Always-Taker", "Defier") ~ 1
      ),
      conditions = list(Z = c(0, 1))
    )
  ) +
  declare_inquiry(
    ATE = mean(Y_D_1 - Y_D_0),
    CACE = mean(Y_D_1[type == "Complier"] - Y_D_0[type == "Complier"])) +
  declare_assignment(Z = conduct_ra(N = N)) +
  declare_measurement(D = reveal_outcomes(D ~ Z),
                      Y = reveal_outcomes(Y ~ D)) +
  declare_estimator(
    Y ~ D | Z,
    .method = iv_robust,
    inquiry = c("ATE", "CACE"),
    label = "Two stage least squares"
  ) +
  declare_estimator(
    Y ~ D,
    .method = lm_robust,
    inquiry = c("ATE", "CACE"),
    label = "As treated"
  ) +
  declare_estimator(
    Y ~ D,
    .method = lm_robust,
    inquiry = c("ATE", "CACE"),
    subset = D == Z,
    label = "Per protocol"
  )
