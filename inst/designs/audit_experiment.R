---
id: audit_experiment
alias: "17.1"
label: audit experiment
category: rdss
keywords: [experiment, descriptive]
description: >
  Audit experiment design. (chapter 17).
packages: [dplyr]
params:
  "N": "Number of units (sample or population size)"
book_link: https://book.declaredesign.org/library/experimental-descriptive.html#def-ch17num1
include_in_shiny: true
---

design <-
  declare_parameters(
    N = 500
  ) +
  declare_model(
    N = N,
    type = sample(
      size = N, 
      replace = TRUE,
      x = c("Always-responder",
            "Anti-Latino discriminator",
            "Never-responder"),
      prob = c(0.30, 0.05, 0.65)
    ),
    # Behavioral assumptions represented here:
    Y_Z_white = if_else(type == "Never-Responder", 0, 1),
    Y_Z_latino = if_else(type == "Always-Responder", 1, 0)
  ) +
  declare_inquiry(
    anti_latino_discrimination = mean(type == "Anti-Latino discriminator")
  ) +
  declare_assignment(Z = complete_ra(N, conditions = c("latino", "white"))) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "anti_latino_discrimination")
