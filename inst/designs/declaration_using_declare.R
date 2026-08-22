---
id: declaration_using_declare
alias: "10.1"
label: Example of declaration using Declare
category: rdss
description: >
  Example of declaration using DeclareDesign (chapter 10).
params:
  "N": "Number of units (sample or population size)"
book_link: https://book.declaredesign.org/declaration-diagnosis-redesign/diagnosing-designs.html#def-ch10num1
include_in_shiny: false
---

design <-
  declare_parameters(
    N = 100
  ) +
  declare_model(
    N = N,
    U = rnorm(N),
    potential_outcomes(Y ~  0.2 * Z + U)
  ) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE")
