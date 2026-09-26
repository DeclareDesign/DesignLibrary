---
id: regression_discontinuity_polynomial
label: Regression discontinuity with a polynomial fit
category: template
keywords: [observational, causal, regression discontinuity]
description: >
  Sharp regression discontinuity. Treatment is assigned when a uniform running
  variable passes the cutoff, and each potential outcome is a polynomial in the
  running variable. Units within the bandwidth are kept and the jump at the
  cutoff is estimated from a polynomial of order poly_reg_order interacted
  with treatment.
  Flattened from DesignLibrary::regression_discontinuity_designer.
params:
  "N": "Number of units"
  "tau": "Jump in the outcome at the cutoff"
  "outcome_sd": "Standard deviation of the outcome shock"
  "cutoff": "Cutoff on the running variable, in (0, 1)"
  "bandwidth": "Largest distance from the cutoff for a unit to be kept"
  "control_coefs": "Polynomial coefficients of the control outcome in the running variable"
  "treatment_coefs": "Polynomial coefficients of the treated outcome in the running variable"
  "poly_reg_order": "Order of the polynomial in the estimator"
include_in_shiny: true
---

po_function <- function(X, coefs, tau) {
  as.vector(poly(X, length(coefs), raw = TRUE) %*% coefs) + tau
}

design <-
  declare_parameters(
    N = 1000,
    tau = 0.15,
    outcome_sd = 0.1,
    cutoff = 0.5,
    bandwidth = 0.5,
    control_coefs = c(0.5, 0.5),
    treatment_coefs = c(-5, 1),
    poly_reg_order = 4
  ) +
  declare_model(
    N = N,
    X = runif(N, 0, 1) - cutoff,
    noise = rnorm(N, 0, outcome_sd),
    Z = 1 * (X > 0),
    Y_Z_0 = po_function(X, tau = 0, coefs = control_coefs) + noise,
    Y_Z_1 = po_function(X, tau = tau, coefs = treatment_coefs) + noise
  ) +

  declare_inquiry(
    LATE = po_function(X = 0, coefs = treatment_coefs, tau = tau) -
      po_function(X = 0, coefs = control_coefs, tau = 0)
  ) +

  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +

  declare_sampling(filter = abs(X) < abs(bandwidth)) +

  declare_estimator(Y ~ poly(X, poly_reg_order) * Z, .method = lm_robust,
                    term = "Z", inquiry = "LATE")
