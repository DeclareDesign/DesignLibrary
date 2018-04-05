#' @export
two_arm_template <- function(N = c(500, 100, 1000, 2000),
                             prob = c(.5,seq(.1,.9,.1)),
                             tau_mean = c(1, .1, .5, 1.5, 2),
                             tau_sd = c(3, 1, .1, .5, 1.5, 2),
                             outcome_sd = c(3, 1, .1, .5, 1.5, 2))
{
  {
    N <- as.numeric(N[1])
    prob <- as.numeric(prob[1])
    tau_mean <- as.numeric(tau_mean[1])
    tau_sd <- as.numeric(tau_sd[1])
    outcome_sd <- as.numeric(outcome_sd[1])
  }
  {{{
        population <- declare_population(
          N = N,
          noise = rnorm(N, sd = outcome_sd),
          treatment_effect = rnorm(N, mean = tau_mean, sd = tau_sd)
        )
        potential_outcomes <- declare_potential_outcomes(Y ~ Z * treatment_effect + noise)
        assignment <- declare_assignment(prob = prob)
        estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
        estimator <- declare_estimator(Y ~ Z, estimand = estimand)
        two_arm <- declare_design(population,
                                  potential_outcomes,
                                  estimand,
                                  assignment,
                                  estimator)
  }}}
  two_arm
}

attr(two_arm_template, "tips") <- c(
  N = "Size of sample",
  prob = "Probability of assignment to treatment",
  tau_mean = "Average treatment effect",
  tau_sd = "Standard deviation of individual treatment effects",
  outcome_sd = "Standard deviation of outcome in control"
)
