---
id: bootstrapped
alias: "9.5"
label: bootstrapped
category: rdss
keywords: [observational, causal, inference]
description: >
  Bootstrapped standard errors. (chapter 9).
packages: [rdss]
book_link: https://book.declaredesign.org/declaration-diagnosis-redesign/choosing-answer-strategy.html#def-ch9num5
include_in_shiny: true
---

design <-
  declare_model(data = resample_data(clingingsmith_etal)) +
  declare_estimator(views ~ success, .method = difference_in_means)
