#' @export
regression_discontinuity_template <- function(
  N = c(1000, 50,100,250, 500, 2500, 5000, 10000),
  tau = c(.15, 0, -1, -.5, -.15,  .5, 1),
  cutoff = c(.5, .01, .1, .25, .75, .9, .99),
  bandwidth = c(.5, .01, .1, .25, .75, .9, .99),
  poly_order = c(4, 3, 2, 1, 8)
){
  {
    N = as.numeric(N[1])
    tau = as.numeric(tau[1])
    cutoff = as.numeric(cutoff[1])
    bandwidth = as.numeric(bandwidth[1])
    poly_order = as.numeric(poly_order[1])
  }
  {{{
    # Model -------------------------------------------------------------------
    control <- function(X) {
      as.vector(poly(X, 4, raw = T) %*% c(.7, -.8, .5, 1))}
    treatment <- function(X) {
      as.vector(poly(X, 4, raw = T) %*% c(0, -1.5, .5, .8)) + tau}
    population <- declare_population(
      N = N,
      X = runif(N,0,1) - cutoff,
      noise = rnorm(N,0,.1),
      Z = 1 * (X > 0))
    potential_outcomes <- declare_potential_outcomes(
      Y_Z_0 = control(X) + noise,
      Y_Z_1 = treatment(X) + noise)

    # Inquiry -----------------------------------------------------------------
    estimand <- declare_estimand(LATE = treatment(0) - control(0))

    # Data Strategy -----------------------------------------------------------
    sampling <- declare_sampling(handler = function(data){
      subset(data,(X > 0 - bandwidth) & X < 0 + bandwidth)})

    # Answer Strategy ---------------------------------------------------------
    estimator <- declare_estimator(
      formula = Y ~ poly(X, poly_order) * Z,
      model = lm_robust,
      coefficients = "Z",
      estimand = estimand)

    # Design ------------------------------------------------------------------
    rd_design <- declare_design(
      population, potential_outcomes, estimand, declare_reveal(Y), sampling, estimator)
  }}}
  rd_design
}
attr(regression_discontinuity_template,"tips") <-
  c(N = "Size of population to sample from",
    tau = "Difference in potential outcomes functions at the threshold",
    cutoff = "Threshold on running variable beyond which units are treated",
    bandwidth = "Bandwidth around threshold from which to include units",
    poly_order = "Order of the polynomial regression used to estimate the jump at the cutoff"
  )
