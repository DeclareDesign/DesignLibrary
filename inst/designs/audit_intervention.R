---
id: audit_intervention
alias: "17.2"
label: audit intervention
category: rdss
keywords: [experiment, descriptive]
description: >
  Audit experiment intervention study design. (chapter 17).
packages: [dplyr]
params:
  "N": "Number of units (sample or population size)"
book_link: https://book.declaredesign.org/library/experimental-descriptive.html#def-ch17num2
include_in_shiny: true
---

design <-
  declare_parameters(
    N = 5000
  ) +
  # This part of the design is about causal inference
  declare_model(
    N = N,
    type_D_0 = sample(
      size = N,
      replace = TRUE,
      x = c("Always-Responder",
            "Anti-Latino Discriminator",
            "Never-Responder"),
      prob = c(0.30, 0.05, 0.65)
    ),
    type_tau_i = rbinom(N, 1, 0.5),
    type_D_1 = if_else(
      type_D_0 == "Anti-Latino Discriminator" &
        type_tau_i == 1,
      "Always-Responder",
      type_D_0
    )
  ) +
  declare_inquiry(
    ATE = mean((type_D_1 == "Anti-Latino Discriminator") -
                 (type_D_0 == "Anti-Latino Discriminator"))
  ) +
  declare_assignment(D = complete_ra(N)) +
  declare_measurement(type = reveal_outcomes(type ~ D)) +
  # This part is about descriptive inference in each condition!
  declare_model(
    Y_Z_white = if_else(type == "Never-Responder", 0, 1),
    Y_Z_latino = if_else(type == "Always-Responder", 1, 0)
  ) +
  declare_assignment(
    Z = complete_ra(N, conditions = c("latino", "white"))) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z * D, term = "Zwhite:D", inquiry = "ATE")
