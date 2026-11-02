---
id: multilevel_answer_strategies
alias: "15.5"
label: Redesign over answer strategies.
category: rdss
keywords: [observational, descriptive, sampling, survey]
description: >
  Redesign over answer strategies. (chapter 15).
packages: [rdss, lme4, dplyr, tibble]
params:
  "states": "State-level population frame used to build the sample"
book_link: https://book.declaredesign.org/library/observational-descriptive.html#def-ch15num5
include_in_shiny: false
---

states <- 
  as_tibble(state.x77) |>
  transmute(
    state = rownames(state.x77),
    prop_of_US = Population / sum(Population),
    # results in exactly 2,000 due to rounding
    state_n = round(prop_of_US * 1998.6), 
    prob_HS = `HS Grad` / 100,
    state_shock = rnorm(n = n(), sd = 0.5),
    state_mean = prob_HS * pnorm(0.2 + state_shock) + (1 - prob_HS) * pnorm(state_shock)
  )

design <-
  declare_model(
    data = states[rep(1:50, states$state_n), ],
    HS = rbinom(n = N, size = 1, prob = prob_HS),
    PS_weight =
      case_when(HS == 0 ~ (1 - prob_HS),
                HS == 1 ~ prob_HS),
    individual_shock = rnorm(n = N, sd = 0.5),
    policy_support = 
      rbinom(N, 1, prob = pnorm(0.2 * HS + individual_shock + state_shock))
  ) +
  declare_inquiry(
    handler = function(data) {
      states |> transmute(
        state, 
        inquiry = "mean_policy_support", 
        estimand = state_mean
      )
    }
  ) +
  declare_estimator(handler = label_estimator(function(data) {
    model_fit <- glmer(
      formula = policy_support ~ HS + (1 | state),
      data = data,
      family = binomial(link = "logit")
    )
    post_stratification_helper(model_fit, data = data, group = state, weights = PS_weight)
  }),
  label = "Partial pooling",
  inquiry = "mean_policy_support") +
  
  declare_estimator(
    handler = label_estimator(function(data) {
      model_fit <- lm_robust(
        formula = policy_support ~ HS + state,
        data = data
      )
      post_stratification_helper(model_fit, data = data, group = state, weights = PS_weight)
    }),
    label = "No pooling",
    inquiry = "mean_policy_support") +
  declare_estimator(
    handler = label_estimator(function(data) {
      model_fit <- lm_robust(
        formula = policy_support ~ HS,
        data = data
      )
      post_stratification_helper(model_fit, data = data, group = state, weights = PS_weight)
    }),
    label = "Full pooling",
    inquiry = "mean_policy_support")
