---
id: italian_village_bayes
alias: "9.3"
label: Italian village  a la Bayes.
category: rdss
description: >
  Italian village design a la Bayes. (chapter 9).
packages: [rdss, rstanarm]
params:
  "N": "Number of units (sample or population size)"
  "summary_fn": "Function: tidies the Stan fit with exponentiated coefficients (R-only)"
book_link: https://book.declaredesign.org/declaration-diagnosis-redesign/choosing-answer-strategy.html#def-ch9num3
include_in_shiny: false
---


summary_fn <- function(fit) rdss::tidy_stan(fit, exponentiate = TRUE)

design <-
  declare_parameters(
    N = 100
  ) +
  declare_model(N = N, age = sample(0:80, size = N, replace = TRUE)) +
  declare_inquiry(mean_age = mean(age)) +
  declare_sampling(S = complete_rs(N = N, n = 3)) +
  declare_estimator(
    age ~ 1,
    .method = stan_glm,
    family = gaussian(link = "log"),
    prior_intercept = normal(50, 5),
    .summary = summary_fn,
    inquiry = "mean_age"
  )
