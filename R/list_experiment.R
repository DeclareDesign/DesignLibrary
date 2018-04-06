#' @export
list_experiment_template <- function(N = c(1000, 1500, 2000, 2500),
                                     J = c(3, 4, 5),
                                     prevalence_rate = c(0.5, seq(0, 0.5)),
                                     withholding_rate = c(0.1, seq(0, 0.5)))
{
  {
    N <- as.numeric(N[1])
    J <- as.numeric(J[1])
    prevalence_rate <- as.numeric(prevalence_rate[1])
    withholding_rate <- as.numeric(withholding_rate[1])
  }
  {{{
        pop <-
          declare_population(
            N = N,
            sensitive_trait = draw_binary(prob = prevalence_rate, N = N),
            withholder = draw_binary(prob = sensitive_trait * withholding_rate, N = N),
            direct_question = sensitive_trait - withholder,
            control_items = sample(
              0:J,
              N,
              replace = TRUE,
              prob = sample(c(.2, .3), J, replace = TRUE)
            )
          )

        pos <- declare_potential_outcomes(Y ~ control_items + Z * sensitive_trait)

        assignment <- declare_assignment(prob = 0.5)

        true_rate <- declare_estimand(prevalence_rate = prevalence_rate)
        direct_estimator <-
          declare_estimator(
            direct_question ~ 1,
            model = lm_robust,
            estimand = true_rate,
            label = "Direct"
          )
        list_estimator <-
          declare_estimator(Y ~ Z, estimand = true_rate, label = "List")

        list_experiment <-
          declare_design(pop,
                         pos,
                         true_rate,
                         assignment,
                         direct_estimator,
                         list_estimator)

  }}}
  list_experiment
}

attr(list_experiment_template,"tips") <- c(
  N = "Size of sample",
  J = "Number of control items",
  prevalence_rate = "True prevalance rate",
  withholding_rate = "Probability of withholding, given the sensitive trait"
)

