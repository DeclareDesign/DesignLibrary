---
id: simple_random_sampling
alias: "15.1"
label: simple random sampling
category: rdss
keywords: [observational, descriptive]
description: >
  Simple random sampling design (chapter 15).
params:
  "cuts": "Cut points for stratified or grouped sampling"
  "sample_size": "Sample size"
  "data": "Population data frame; pass via make_design(..., data = ...). Not edited in the browser."
book_link: https://book.declaredesign.org/library/observational-descriptive.html#def-ch15num1
include_in_shiny: true
---

set.seed(343) # fix random seed to yield a fixed population of units

portola <-
  fabricate(
    N = 2100,
    Y_star = rnorm(N)
  )
data <- portola

design <-
  declare_parameters(
    cuts = 7,
    sample_size = 100
  ) +
  declare_model(data = data) +
  declare_measurement(Y = as.numeric(cut(Y_star, cuts))) +
  declare_inquiry(Y_bar = mean(Y)) +
  declare_sampling(S = complete_rs(N, n = sample_size)) +
  declare_estimator(Y ~ 1, inquiry = "Y_bar")
