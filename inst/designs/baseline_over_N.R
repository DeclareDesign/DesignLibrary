---
id: baseline_over_N
alias: "11.1"
label: A baseline declaration intended to be reed over $N$.
category: rdss
description: >
  A baseline declaration intended to be redesigned over $N$. (chapter
  11).
packages: [broom]
params:
  "N": "Number of units (sample or population size)"
book_link: https://book.declaredesign.org/declaration-diagnosis-redesign/redesigning.html#def-ch11num1
include_in_shiny: false
---

design <-
  declare_parameters(
    N = 100
  ) +
  declare_model(N = N) +
  declare_measurement(Y = rbinom(n = N, size = 1, prob = 0.55)) +
  declare_test(handler =
                 label_estimator(function(data) {
                   test <- prop.test(x = table(data$Y), p = 0.5)
                   tidy(test)
                 }))
