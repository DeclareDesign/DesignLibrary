---
id: logit_probit_ols
alias: "11.5"
label: Logit, probit, or OLS?
category: rdss
keywords: [experiment, binary, logit, probit, margins]
description: >
  Compare OLS, logit, and probit estimates of an ATE on a binary outcome.
  Marginal effects for GLM estimators use the margins package.
packages: [margins, broom]
params:
  "N": "Number of units (sample or population size)"
  "b": "Treatment effect (outcome scale)"
  "tidy_margins": "Function: tidy average marginal effects from a GLM (R-only)"
diagnosands: rmse, bias
book_link: https://book.declaredesign.org/declaration-diagnosis-redesign/redesigning.html#def-ch11num5
include_in_shiny: true
---



tidy_margins <- function(x) {
  broom::tidy(margins::margins(x, data = x$data), conf.int = TRUE)
}

design <-
  declare_parameters(
    N = 100,
    b = 0.2
  ) +
  declare_model(
    N = N,
    U = rnorm(N),
    potential_outcomes(Y ~ rbinom(N, 1, prob = b * Z + 0.6))
  ) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N, prob = 0.5)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(
    Y ~ Z,
    .method = lm_robust,
    inquiry = "ATE",
    term = "Z",
    label = "OLS"
  ) +
  declare_estimator(
    Y ~ Z,
    .method = glm,
    family = binomial("logit"),
    .summary = tidy_margins,
    inquiry = "ATE",
    term = "Z",
    label = "logit"
  ) +
  declare_estimator(
    Y ~ Z,
    .method = glm,
    family = binomial("probit"),
    .summary = tidy_margins,
    inquiry = "ATE",
    term = "Z",
    label = "probit"
  )
