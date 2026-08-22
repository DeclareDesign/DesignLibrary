---
id: multi_site_studies
alias: "19.4"
label: multi-site studies
category: rdss
keywords: [complex]
description: >
  Multi-site studies with and without coordination. (chapter 19).
packages: [metafor, rdss, dplyr, tidyr, broom]
params:
  "study_assignment_probabilities": "Assignment probabilities by study"
  "study_coordination": "Cross-study coordination parameter"
  "study_intercepts": "Study-specific intercepts"
  "study_priors": "Study-specific prior parameters"
  "study_sizes": "Sample sizes by study"
book_link: https://book.declaredesign.org/library/complex.html#def-ch19num4
include_in_shiny: true
---


# helper function to estimate study level effects

design <-
  declare_parameters(
    study_sizes = c(500, 1000, 1500, 2000, 2500),
    study_assignment_probabilities = c(0.5, 0.5, 0.6, 0.7, 0.8),
    study_intercepts = 1:5,
    study_priors = seq(from = 0, to = 0.3, length = 5),
    study_coordination = "high"  # Change to "low"
  ) +
  
  declare_model(
    sites = add_level(
      N = 5,
      size = study_sizes,
      intercept = study_intercepts,
      tau_1 = rnorm(N, study_priors, 0.1),
      tau_2 = rnorm(N, 0.3, 0.2),
      coordination = study_coordination,
      # if coordinated, take tau_1, otherwise take the bigger of the two
      tau = if_else(coordination == "high",
                    tau_1,
                    pmax(tau_1, tau_2))
    ),
    subjects = add_level(
      N = size, 
      U = rnorm(N)
    )
  ) +
  
  declare_model(
    potential_outcomes(
      Y ~ intercept + tau * Z_implemented + U, 
      conditions = list(Z_implemented = c(0, 1))),
    potential_outcomes(
      Y ~ intercept + tau_1 * Z_common + U, 
      conditions = list(Z_common = c(0, 1)))
  ) + 
  
  declare_inquiries(handler = function(data) {
    data |>
      group_by(sites) |>
      summarize(
        ATE_implemented_site = mean(Y_Z_implemented_1 - Y_Z_implemented_0),
        ATE_common_site = mean(Y_Z_common_1 - Y_Z_common_0)) |> 
      ungroup() |> 
      summarize(ATE_implemented = mean(ATE_implemented_site),
                ATE_common = mean(ATE_common_site)) |> 
      pivot_longer(cols = everything(), names_to = "inquiry", values_to = "estimand")
  }) + 
  
  declare_assignment(
    Z_implemented = block_ra(
      blocks = sites,
      block_prob = study_assignment_probabilities)
  ) +
  
  declare_measurement(Y = reveal_outcomes(Y ~ Z_implemented)) +
  
  declare_step(function(data) {
    data |>
      group_by(sites) |>
      reframe(tidy(lm_robust(Y ~ Z_implemented))) |>
      filter(term == "Z_implemented")
  }) +
  
  declare_estimator(
    yi = estimate,
    sei = std.error,
    method = "REML",
    .method = rma_helper,
    .summary = rma_mu_tau,
    term = "mu",
    inquiry = c("ATE_implemented", "ATE_common")
  )
