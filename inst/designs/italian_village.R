---
id: italian_village
alias: "9.1"
label: Italian village
category: rdss
description: >
  Italian village design. (chapter 9).
params:
  "N": "Number of units (sample or population size)"
book_link: https://book.declaredesign.org/declaration-diagnosis-redesign/choosing-answer-strategy.html#def-ch9num1
include_in_shiny: false
---

design <-
  declare_parameters(
    N = 100
  ) +
  declare_model(N = N, age = sample(0:80, size = N, replace = TRUE)) +
  declare_inquiry(mean_age = mean(age)) +
  declare_sampling(S = complete_rs(N = N, n = 3)) +
  declare_estimator(age ~ 1, .method = lm_robust)
