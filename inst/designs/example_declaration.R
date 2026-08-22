---
id: example_declaration
alias: "5.1"
label: Example declaration
category: rdss
description: >
  Example declaration (chapter 5).
params:
  "N": "Number of units (sample or population size)"
book_link: https://book.declaredesign.org/declaration-diagnosis-redesign/declaring-designs.html#def-ch5num1
include_in_shiny: false
---

design <-
  declare_parameters(
    N = 1000
  ) +
  declare_model(
    N = N,
    U = rnorm(N),
    X = rbinom(N, 1, prob = pnorm(U)),
    Y = rbinom(N, 1, prob = pnorm(U + X))
  ) +
  declare_inquiry(Ybar = mean(Y[X == 1])) +
  declare_sampling(S = simple_rs(N, prob = 0.1)) +
  declare_estimator(Y ~ 1,
                    .method = lm_robust,
                    subset = X == 1,
                    inquiry = "Ybar")
