---
id: pate_with_sampling
alias: "4.1"
label: PATE with sampling
category: rdss
keywords: [sampling, experiment, PATE]
description: >
  Sample from a population, then randomly assign treatment. Inquiry is the
  population average treatment effect (PATE).
params:
  "N": "Number of units (sample or population size)"
  "n": "Sample size drawn from the population (when N is population size)"
  "b": "Treatment effect (outcome scale)"
  "prob": "Probability of assignment to treatment"
book_link: https://book.declaredesign.org/declaration-diagnosis-redesign/declaring-designs.html
include_in_shiny: true
---

design <-
  declare_parameters(
    N = 100,
    n = 50,
    b = 0.25,
    prob = 0.5
  ) +
  declare_model(
    N = N,
    U = rnorm(N),
    potential_outcomes(Y ~ b * Z + U)
  ) +
  declare_inquiry(PATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_sampling(S = complete_rs(N, n = n)) +
  declare_assignment(Z = complete_ra(N, prob = prob)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(
    Y ~ Z,
    .method = difference_in_means,
    inquiry = "PATE"
  )
