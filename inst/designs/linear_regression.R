---
id: linear_regression
alias: "13.1"
label: Linear regression
category: rdss
description: >
  Linear regression design (chapter 13).
params:
  "N": "Number of units (sample or population size)"
book_link: https://book.declaredesign.org/declaration-diagnosis-redesign/declaration-in-code.html#def-ch13num1
include_in_shiny: false
---

design <-
  declare_parameters(
    N = 100
  ) +
  declare_model(N = N,
                U = rnorm(N),
                potential_outcomes(Y ~ 0.2 * Z + U)) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(
    Y ~ Z,
    .method = lm_robust,
    .summary = tidy,
    term = "Z",
    inquiry = "ATE",
    label = "OLS"
  )
