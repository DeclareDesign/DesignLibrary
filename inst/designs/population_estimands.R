---
id: population_estimands
alias: "7.1"
label: population estimands
category: rdss
description: >
  Super-population, population, and finite sample design (chapter 7).
params:
  "N": "Number of units (sample or population size)"
  "n": "Sample size drawn from the population (when N is population size)"
  "superpopulation_mean": "Superpopulation mean of the estimand"
book_link: https://book.declaredesign.org/declaration-diagnosis-redesign/defining-inquiry.html#def-ch7num1
include_in_shiny: true
---

# The same estimator might be used for estimands defined with
# respect to different groups, with implications for coverage

design <-
  declare_parameters(
    N = 20,
    n = 10,
    superpopulation_mean = 1
  ) +
  declare_model(
    N = N, 
    Y = 1 + rnorm(N)
  ) +
  declare_inquiry(
    superpopulation_mean = superpopulation_mean,
    population_mean = mean(Y)
  ) + 
  declare_sampling(
    S = complete_rs(N, n = n)
  ) +
  declare_inquiry(
    sample_mean = mean(Y)
  ) + 
  declare_estimator(Y ~ 1)
