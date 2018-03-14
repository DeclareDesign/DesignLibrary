#' @export
list_experiment_template <- function(N = c(1000, 1500, 2000, 2500),
                                     prevalence_rate = c(0.5, seq(0, 0.5)),
                                     withholding_rate = c(0.1, seq(0, 0.5)))
{
  {
    N <- as.numeric(N[1])
    prevalence_rate <- as.numeric(prevalence_rate[1])
    withholding_rate <- as.numeric(withholding_rate[1])
  }
  {
    {
      {
        pop <-
          declare_population(
            N = N,
            sensitive_trait = draw_binary(prob = prevalence_rate, N = N),
            withholder = draw_binary(prob = sensitive_trait * withholding_rate, N = N),
            direct_question = sensitive_trait - withholder,
            control_items = sample(
              0:3,
              N,
              replace = TRUE,
              prob = c(0.2, .3, .3, .2)
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
        
      }
    }
  }
  list_experiment
}

attr(list_experiment_template,"tips") <- c(
  N = "Size of sample",
  prevalence_rate = "True prevalance rate",
  withholding_rate = "Probability of withholding, given the sensitive trait"
)

