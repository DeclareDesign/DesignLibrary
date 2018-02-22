#' @export
two_arm_template <- function(
  N = c(500, 100, 1000, 2000),
  n = c(250, 50, 500, 1000),
  m = c(100, 10, 30, 50, 75, 150, 200),
  tau = c(1, .1, .5, 1.5, 2),
  sigma = c(3, 1, .1, .5, 1.5, 2) )
{
  {
    N <- as.numeric(N[1])
    n <- as.numeric(n[1])
    m <- as.numeric(m[1])
    tau <- as.numeric(tau[1])
    sigma<- as.numeric(sigma[1])
    if(n > N) stop("n > N"); as.numeric(0)
    if(m > n) stop("m > n"); as.numeric(0)
  }
  {{{
    population <- declare_population(
      N = N, noise = rnorm(N), treatment_effect = rnorm(N, mean = tau, sd = sigma))
    potential_outcomes <- declare_potential_outcomes(
      Y_Z_0 = noise,
      Y_Z_1 = noise + treatment_effect)
    sampling <- declare_sampling(n = n)
    assignment <- declare_assignment(m = m)
    estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
    estimator <- declare_estimator(Y ~ Z, estimand = estimand)
    two_arm <- declare_design(
      population, potential_outcomes, sampling, estimand, assignment, reveal_outcomes, estimator)
  }}}
  two_arm
}

attr(two_arm_template,"tips") <- c(
  N = "Size of population",
  n = "Size of sample",
  m = "Number assigned to treatment",
  tau = "Average treatment effect",
  sigma = "Additional (square root of) variance added by treatment"
)
