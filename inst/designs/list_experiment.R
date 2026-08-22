---
id: list_experiment
alias: "17.3"
label: list experiment
category: rdss
keywords: [experiment, descriptive]
description: >
  List experiment design. (chapter 17).
params:
  "N": "Number of units (sample or population size)"
book_link: https://book.declaredesign.org/library/experimental-descriptive.html#def-ch17num3
include_in_shiny: true
---

design <-
  declare_parameters(
    N = 500
  ) +
  declare_model(
    N = N,
    control_count = rbinom(N, size = 3, prob = 0.5),
    Y_star = rbinom(N, size = 1, prob = 0.3),
    potential_outcomes(Y_list ~ Y_star * Z + control_count) 
  ) +
  declare_inquiry(prevalence_rate = mean(Y_star)) +
  declare_assignment(Z = complete_ra(N)) + 
  declare_measurement(Y_list = reveal_outcomes(Y_list ~ Z)) +
  declare_estimator(Y_list ~ Z, .method = difference_in_means, 
                    inquiry = "prevalence_rate")
