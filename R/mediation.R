#' @export
mediation_template <- function(N = c(100, 30, 500, 1000),
                               a = seq(from = .5, to = -.5, by = -.1),
                               b = seq(from = .5, to = -.5, by = -.1),
                               d = seq(from = .5, to = -.5, by = -.1),
                               rho = c(.2, seq(from = -1, to = 1, by = .1))
                               )
{
  {
    N <- as.numeric(N[1])
    a <- as.numeric(a[1])
    b <- as.numeric(b[1])
    d <- as.numeric(d[1])
    rho <- as.numeric(rho[1])
    if(rho < -1 | rho > 1) stop("rho must be in [-1, 1]")
  }
  {{{
    pop <- declare_population(
      N = N, 
      e1 = rnorm(N),
      e2 = rnorm(n = N, mean = rho * e1, sd = 1 - rho^2)
    )
    pos_M <-
      declare_potential_outcomes(M ~ a * Z + e1)
    pos_Y <-
      declare_potential_outcomes(Y ~ d * Z + b * M + e2)
    assignment <- declare_assignment(prob = 0.5)
    mand_a <- declare_estimand(a = a)
    mand_b <- declare_estimand(b = b)
    mand_d <- declare_estimand(d = d)
    mediator_regression <- declare_estimator(
      M ~ Z,
      model = lm_robust,
      coefficients = "Z",
      estimand = mand_a,
      label = "Mediator regression"
    )
    outcome_regression <- declare_estimator(
      Y ~ Z + M,
      model = lm_robust,
      coefficients = c("M","Z"),
      estimand = c(mand_b,mand_d),
      label = "Outcome regression"
    )
    mediation <-
      declare_design(
        pop,
        pos_M,
        assignment,
        pos_Y,
        mand_a,
        mand_b,
        mand_d,
        mediator_regression,
        outcome_regression
      )
  }}}
  mediation
}
attr(mediation_template,"tips") <- c(
  N = "Size of sample",
  a = "Effect of treatment (Z) on mediatior (M)",
  b = "Effect of mediatior (M) on outcome (Y)",
  d = "Direct effect of treatment (Z) on outcome (Y)",
  rho = "Correlation of mediator (M) and outcome (Y) error terms"
)
