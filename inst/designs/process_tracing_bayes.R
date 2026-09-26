---
id: process_tracing_bayes
label: Process tracing with two clues
category: template
keywords: [observational, causal, qualitative, bayesian]
description: >
  One case with X = 1 and Y = 1 is selected from a population of causal
  processes, and two clues are observed whose probabilities depend on whether
  X caused Y (a straw in the wind and a smoking gun at the defaults). The
  design compares the posterior that X caused Y after no clue,
  either clue alone, and both clues, each updated by Bayes' rule from prior_H.
  Flattened from DesignLibrary::process_tracing_designer.
params:
  "N": "Number of cases in the population"
  "prob_X": "Probability that X = 1"
  "process_proportions": "Shares of X causes Y, Y regardless, X causes not Y, and not Y regardless"
  "prior_H": "Prior probability that X caused Y"
  "p_E1_H": "Probability of clue 1 if X caused Y"
  "p_E1_not_H": "Probability of clue 1 if X did not cause Y"
  "p_E2_H": "Probability of clue 2 if X caused Y"
  "p_E2_not_H": "Probability of clue 2 if X did not cause Y"
  "cor_E1E2_H": "Correlation of the two clues if X caused Y"
  "cor_E1E2_not_H": "Correlation of the two clues if X did not cause Y"
include_in_shiny: true
---

# Probabilities of the clue pairs 00, 01, 10, and 11
joint_prob <- function(p1, p2, rho) {
  r <- rho * (p1 * p2 * (1 - p1) * (1 - p2))^0.5
  c(p00 = (1 - p1) * (1 - p2) + r,
    p01 = p2 * (1 - p1) - r,
    p10 = p1 * (1 - p2) - r,
    p11 = p1 * p2 + r)
}

bayes_rule <- function(p_H, p_E_H, p_E_not_H) {
  p_E_H * p_H / (p_E_H * p_H + p_E_not_H * (1 - p_H))
}

prior_only <- function(data, prior_H) {
  data.frame(estimate = bayes_rule(p_H = prior_H, p_E_H = 1, p_E_not_H = 1),
             result = "TRUE")
}

# Handler arguments carry the parameter names, which is how a redesign
# reaches them
clue_posterior <- function(E, prior_H, p_E_H, p_E_not_H) {
  data.frame(
    estimate = bayes_rule(
      p_H = prior_H,
      p_E_H = ifelse(E, p_E_H, 1 - p_E_H),
      p_E_not_H = ifelse(E, p_E_not_H, 1 - p_E_not_H)
    ),
    result = as.character(E)
  )
}

E1_only <- function(data, prior_H, p_E1_H, p_E1_not_H) {
  clue_posterior(data$E1, prior_H, p_E1_H, p_E1_not_H)
}

E2_only <- function(data, prior_H, p_E2_H, p_E2_not_H) {
  clue_posterior(data$E2, prior_H, p_E2_H, p_E2_not_H)
}

both_clues <- function(data, prior_H, p_E1_H, p_E2_H, cor_E1E2_H,
                       p_E1_not_H, p_E2_not_H, cor_E1E2_not_H) {
  observed <- c("00", "01", "10", "11") %in% data$test_results
  data.frame(
    estimate = bayes_rule(
      p_H = prior_H,
      p_E_H = joint_prob(p_E1_H, p_E2_H, cor_E1E2_H)[observed],
      p_E_not_H = joint_prob(p_E1_not_H, p_E2_not_H, cor_E1E2_not_H)[observed]
    ),
    result = data$test_results
  )
}

design <-
  declare_parameters(
    N = 100,
    prob_X = 0.5,
    process_proportions = c(0.25, 0.25, 0.25, 0.25),
    prior_H = 0.5,
    p_E1_H = 0.8,
    p_E1_not_H = 0.2,
    p_E2_H = 0.3,
    p_E2_not_H = 0,
    cor_E1E2_H = 0,
    cor_E1E2_not_H = 0
  ) +
  declare_model(
    N = N,
    causal_process = sample(
      x = c("X_causes_Y", "Y_regardless", "X_causes_not_Y", "not_Y_regardless"),
      size = N,
      replace = TRUE,
      prob = process_proportions
    ),
    X = rbinom(N, 1, prob_X) == 1,
    Y = (X & causal_process == "X_causes_Y") |
      (!X & causal_process == "X_causes_not_Y") |
      (causal_process == "Y_regardless")
  ) +

  declare_sampling(
    S = strata_rs(strata = paste(X, Y),
                  strata_n = c("X0Y0" = 0, "X0Y1" = 0, "X1Y0" = 0, "X1Y1" = 1))
  ) +

  declare_measurement(
    test_results = sample(
      c("00", "01", "10", "11"), 1,
      prob = ifelse(rep(causal_process == "X_causes_Y", 4),
                    joint_prob(p_E1_H, p_E2_H, cor_E1E2_H),
                    joint_prob(p_E1_not_H, p_E2_not_H, cor_E1E2_not_H))
    ),
    E1 = test_results == "10" | test_results == "11",
    E2 = test_results == "01" | test_results == "11"
  ) +

  declare_inquiry(did_X_cause_Y = causal_process == "X_causes_Y") +

  declare_estimator(handler = label_estimator(prior_only), prior_H = prior_H,
                    label = "No tests (Prior)", inquiry = "did_X_cause_Y") +
  declare_estimator(handler = label_estimator(E1_only), prior_H = prior_H,
                    p_E1_H = p_E1_H, p_E1_not_H = p_E1_not_H,
                    label = "Straw in the Wind", inquiry = "did_X_cause_Y") +
  declare_estimator(handler = label_estimator(E2_only), prior_H = prior_H,
                    p_E2_H = p_E2_H, p_E2_not_H = p_E2_not_H,
                    label = "Smoking Gun", inquiry = "did_X_cause_Y") +
  declare_estimator(handler = label_estimator(both_clues), prior_H = prior_H,
                    p_E1_H = p_E1_H, p_E2_H = p_E2_H, cor_E1E2_H = cor_E1E2_H,
                    p_E1_not_H = p_E1_not_H, p_E2_not_H = p_E2_not_H,
                    cor_E1E2_not_H = cor_E1E2_not_H,
                    label = "Straw in the Wind and Smoking Gun",
                    inquiry = "did_X_cause_Y")
