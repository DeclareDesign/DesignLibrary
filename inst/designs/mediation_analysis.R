---
id: mediation_analysis
label: Mediation analysis
category: template
keywords: [experiment, causal, mediation]
description: >
  Two-arm trial with a binary mediator, declaring the first stage, the
  controlled direct effects, the natural direct effects, and the effect of
  the mediator on the outcome at each level of treatment as separate
  inquiries, and answering all of them with the usual
  regression of the outcome on the treatment interacted with the mediator.
  The design's use is to show when that answer strategy fails: the mediator
  is not randomly assigned, and rho governs how far the mediator's shock and
  the outcome's shock move together. At rho = 0 the mediator is unconfounded
  by construction and the regressions recover their targets. Raise rho and
  the estimates for every quantity defined through the mediator drift from
  their estimands while the first stage stays unbiased.
  Ported from DesignLibrary::mediation_analysis_designer.
params:
  "N": "Number of units"
  "a": "Effect of treatment on the latent index for the mediator"
  "b": "Effect of the mediator on the outcome"
  "c": "Interaction of the mediator and treatment in the outcome"
  "d": "Direct effect of treatment on the outcome"
  "rho": "Correlation between the mediator and outcome shocks, in [-1, 1]. Nonzero means the mediator is confounded."
include_in_shiny: true
---

design <-
  declare_parameters(
    N = 200,
    a = 1,
    b = 0.4,
    c = 0,
    d = 0.5,
    rho = 0
  ) +
  declare_model(
    N = N,
    e1 = rnorm(N),
    e2 = rnorm(n = N, mean = rho * e1, sd = sqrt(1 - rho^2)),
    potential_outcomes(M ~ 1 * (a * Z + e1 > 0)),
    potential_outcomes(Y ~ d * Z + b * M + c * M * Z + e2,
                       conditions = list(M = 0:1, Z = 0:1)),
    Y_nat0_Z_0 = b * M_Z_0 + e2,
    Y_nat0_Z_1 = d + b * M_Z_0 + c * M_Z_0 + e2,
    Y_nat1_Z_0 = b * M_Z_1 + e2,
    Y_nat1_Z_1 = d + b * M_Z_1 + c * M_Z_1 + e2
  ) +

  declare_inquiry(
    FirstStage = mean(M_Z_1 - M_Z_0),
    Mediator_Effect_0 = mean(Y_M_1_Z_0 - Y_M_0_Z_0),
    Mediator_Effect_1 = mean(Y_M_1_Z_1 - Y_M_0_Z_1),
    Controlled_Direct_0 = mean(Y_M_0_Z_1 - Y_M_0_Z_0),
    Controlled_Direct_1 = mean(Y_M_1_Z_1 - Y_M_1_Z_0),
    Natural_Direct_0 = mean(Y_nat0_Z_1 - Y_nat0_Z_0),
    Natural_Direct_1 = mean(Y_nat1_Z_1 - Y_nat1_Z_0)
  ) +

  declare_assignment(Z = complete_ra(N, prob = 0.5)) +

  declare_measurement(
    M = reveal_outcomes(M ~ Z),
    Y = reveal_outcomes(Y ~ M + Z),
    Y_nat0 = reveal_outcomes(Y_nat0 ~ Z),
    Y_nat1 = reveal_outcomes(Y_nat1 ~ Z),
    Not_M = 1 - M
  ) +

  declare_estimator(M ~ Z, .method = lm_robust, inquiry = "FirstStage",
                    label = "Stage 1") +
  declare_estimator(Y ~ Z * M, .method = lm_robust, term = "M",
                    inquiry = "Mediator_Effect_0", label = "Stage 2") +
  declare_estimator(Y ~ Z * M, .method = lm_robust, term = "Z",
                    inquiry = c("Controlled_Direct_0", "Natural_Direct_0"),
                    label = "Direct_0") +
  declare_estimator(Y ~ Z * Not_M, .method = lm_robust, term = "Z",
                    inquiry = c("Controlled_Direct_1", "Natural_Direct_1"),
                    label = "Direct_1")
