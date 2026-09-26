---
id: two_arm_covariate
label: Two-arm trial with a possibly prognostic covariate
category: template
keywords: [experiment, causal, covariates, confounding]
description: >
  Two-arm trial with a covariate W. W can predict the outcome (rho_WY), can
  moderate the effect (h), and can predict assignment (rho_WZ), so the same
  design shows precision gains from adjustment when assignment is random and
  bias from confounding when it is not. Compares difference-in-means, OLS with
  W, and the Lin estimator.
  Flattened from DesignLibrary::two_arm_covariate_designer.
params:
  "N": "Number of units"
  "prob": "Probability of assignment to treatment"
  "control_mean": "Average outcome in control"
  "sd": "Standard deviation of the outcome shock"
  "ate": "Average treatment effect at W = 0"
  "h": "Heterogeneity of the treatment effect by W"
  "rho_WY": "Correlation between W and the outcome shock, in [-1, 1]"
  "rho_WZ": "Correlation between W and the latent assignment variable, in [-1, 1]"
include_in_shiny: true
---

design <-
  declare_parameters(
    N = 100,
    prob = 0.5,
    control_mean = 0,
    sd = 1,
    ate = 1,
    h = 0,
    rho_WY = 0,
    rho_WZ = 0
  ) +
  declare_model(
    N = N,
    W = rnorm(N),
    u_Y = rnorm(N, mean = rho_WY * W, sd = sqrt(1 - rho_WY^2)),
    u_Z = rnorm(N, mean = rho_WZ * W, sd = sqrt(1 - rho_WZ^2)),
    potential_outcomes(Y ~ control_mean + sd * u_Y + Z * (ate + h * W))
  ) +

  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +

  declare_assignment(Z = as.numeric(u_Z < qnorm(prob))) +

  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +

  declare_estimator(Y ~ Z, inquiry = "ATE", label = "No controls") +
  declare_estimator(Y ~ Z + W, term = "Z", .method = lm_robust,
                    inquiry = "ATE", label = "With controls") +
  declare_estimator(Y ~ Z, covariates = ~W, .method = lm_lin,
                    inquiry = "ATE", label = "Lin")
