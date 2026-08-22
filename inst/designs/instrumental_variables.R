---
id: instrumental_variables
alias: "16.4"
label: instrumental variables
category: rdss
keywords: [observational, causal]
description: >
  Instrumental variables design. (chapter 16).
packages: [dplyr]
params:
  "N": "Number of units (sample or population size)"
  "a": "First-stage / instrument effect (or intercept; see design)"
  "b": "Treatment effect (outcome scale)"
  "prob": "Probability of assignment to treatment"
book_link: https://book.declaredesign.org/library/observational-causal.html#def-ch16num4
include_in_shiny: true
---

design <-
  declare_parameters(
    N = 100,
    prob = .5,
    a = 0.25,
    b = 0.1
  ) +
  declare_model(
    N = N, 
    U = rnorm(N),
    potential_outcomes(D ~ if_else(Z + U > 0, 1, 0), 
                       conditions = list(Z = c(0, 1))), 
    potential_outcomes(Y ~ a + b * D +  U, 
                       conditions = list(D = c(0, 1))),
    complier = D_Z_1 == 1 & D_Z_0 == 0
  ) + 
  declare_inquiry(
    ATE  = mean(Y_D_1 - Y_D_0),
    LATE = mean(Y_D_1 - Y_D_0), subset = complier == TRUE) + 
  declare_assignment(Z = complete_ra(N, prob = prob)) +
  declare_measurement(D = reveal_outcomes(D ~ Z),
                      Y = reveal_outcomes(Y ~ D)) + 
  declare_estimator(Y ~ D | Z, .method = iv_robust)
