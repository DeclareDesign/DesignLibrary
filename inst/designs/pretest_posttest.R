---
id: pretest_posttest
label: Pretest-posttest design
category: template
keywords: [experiment, causal, panel]
description: >
  Two-arm trial measuring the outcome before and after treatment, with
  attrition at the second measurement. Three answer strategies are compared:
  a change score, a posttest regression conditioning on the pretest, and a
  posttest-only difference in means.
  Flattened from DesignLibrary::pretest_posttest_designer.
params:
  "N": "Number of units"
  "ate": "Average treatment effect"
  "sd_1": "Standard deviation of the pretest shock"
  "sd_2": "Standard deviation of the posttest shock"
  "rho": "Correlation between the pretest and posttest shocks, in [-1, 1]"
  "attrition_rate": "Share of units not observed at the posttest, in [0, 1]"
include_in_shiny: true
---

design <-
  declare_parameters(
    N = 100,
    ate = 0.25,
    sd_1 = 1,
    sd_2 = 1,
    rho = 0.5,
    attrition_rate = 0.1
  ) +
  declare_model(
    N = N,
    u_t1 = rnorm(N) * sd_1,
    u_t2 = rnorm(N, rho * scale(u_t1), sqrt(1 - rho^2)) * sd_2,
    Y_t1 = u_t1,
    potential_outcomes(Y_t2 ~ u_t2 + ate * Z)
  ) +

  declare_inquiry(ATE = mean(Y_t2_Z_1 - Y_t2_Z_0)) +

  declare_assignment(Z = complete_ra(N)) +

  declare_measurement(
    Y_t2 = reveal_outcomes(Y_t2 ~ Z),
    R = complete_ra(N, prob = 1 - attrition_rate),
    difference = Y_t2 - Y_t1
  ) +

  declare_estimator(difference ~ Z, .method = lm_robust, inquiry = "ATE",
                    subset = R == 1, label = "Change score") +
  # term = "Z" is not in the original designer, and without it the Y_t1
  # coefficient is also matched against the ATE and reports a bias of 0.26
  declare_estimator(Y_t2 ~ Z + Y_t1, .method = lm_robust, term = "Z",
                    inquiry = "ATE", subset = R == 1,
                    label = "Condition on pretest") +
  declare_estimator(Y_t2 ~ Z, .method = lm_robust, inquiry = "ATE",
                    label = "Posttest only")
