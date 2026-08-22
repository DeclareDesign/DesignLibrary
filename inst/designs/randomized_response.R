---
id: randomized_response
label: Randomized response
category: template
keywords: [experiment, descriptive, sensitive]
description: >
  Forced randomized response for a sensitive trait. A coin decides whether a
  respondent answers truthfully or is forced to say yes, so the prevalence
  rate is recovered from the marginal share of yes answers. The direct
  question is estimated alongside it and is biased downward whenever holders
  of the trait withhold.
  Flattened from DesignLibrary::randomized_response_designer.
params:
  "N": "Number of units"
  "prob_forced_yes": "Probability a respondent is forced to answer yes, in [0, 1]"
  "prevalence_rate": "Share of units holding the sensitive trait, in [0, 1]"
  "withholding_rate": "Share of trait holders who deny it under direct questioning, in [0, 1]"
# both estimators return an estimate with no standard error, so power and
# coverage are undefined for this design
diagnosands: [bias, rmse]
include_in_shiny: true
---

design <-
  declare_parameters(
    N = 1000,
    prob_forced_yes = 0.6,
    prevalence_rate = 0.1,
    withholding_rate = 0.5
  ) +
  declare_model(
    N = N,
    sensitive_trait = draw_binary(prob = prevalence_rate, N = N),
    withholder = draw_binary(prob = sensitive_trait * withholding_rate, N = N),
    direct_answer = sensitive_trait - withholder,
    Y_Z_Yes = 1,
    Y_Z_Truth = sensitive_trait
  ) +

  declare_inquiry(true_rate = mean(sensitive_trait)) +

  declare_assignment(
    Z = complete_ra(N, prob = prob_forced_yes, conditions = c("Truth", "Yes"))
  ) +

  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +

  # prob_forced_yes is passed as a step argument rather than closed over, so
  # redesign() rebinds it here as well as in the assignment. A handler that
  # captured it from the file would keep the old value and the estimator would
  # silently divide by the wrong number.
  declare_estimator(
    handler = label_estimator(function(data, prob_forced_yes) {
      data.frame(estimate = (mean(data$Y) - prob_forced_yes) / (1 - prob_forced_yes))
    }),
    prob_forced_yes = prob_forced_yes,
    inquiry = "true_rate",
    label = "Forced Randomized Response"
  ) +
  declare_estimator(
    handler = label_estimator(function(data) {
      data.frame(estimate = mean(data$direct_answer))
    }),
    inquiry = "true_rate",
    label = "Direct Question"
  )
