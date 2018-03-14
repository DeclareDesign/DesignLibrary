#' @export
randomized_response_template <- function(
  N = c(1000, 1500, 2000, 2500),
  prob_forced_yes = c(.6,seq(.1,.9,.1)),
  prevalence_rate = c(.1,seq(.05,.95,.1)),
  withholding_rate = c(.5,seq(.05,.95,.1))
){
  {
    N <- as.numeric(N[1])
    prob_forced_yes <- as.numeric(prob_forced_yes[1])
    prevalence_rate <- as.numeric(prevalence_rate[1])
    withholding_rate <- as.numeric(withholding_rate[1])  
  }
  {{{
    population <- declare_population(
      N = N,
      sensitive_trait = draw_binary(prob = prevalence_rate, N = N),
      withholder = draw_binary(prob = sensitive_trait * withholding_rate, N = N), 
      direct_answer =  sensitive_trait - withholder
    )
    coin_flip <- declare_assignment(
      prob = prob_forced_yes,
      condition_names = c("Truth","Yes")
    )
    potential_outcomes <- declare_potential_outcomes(
      Y_Z_Yes = 1,
      Y_Z_Truth = sensitive_trait 
    )
    estimand <- declare_estimand(true_rate = prevalence_rate)
    randomized_response_estimator <- declare_estimator(
      handler = tidy_estimator(
        function(data) with(
          data, 
          data.frame(est = (mean(Y) - prob_forced_yes) / (1 - prob_forced_yes)))),
      estimand = estimand,
      label = "Forced Response"
    )
    direct_question_estimator <- declare_estimator(
      handler = tidy_estimator(function(data) with(
        data, 
        data.frame(est = mean(direct_answer)))),
      estimand = estimand,
      label = "Direct Question"
    )
    randomized_response <- declare_design(
      population,
      coin_flip,
      potential_outcomes,
      estimand,
      declare_reveal(Y,Z),
      randomized_response_estimator,
      direct_question_estimator
    )
  }}}
  randomized_response
}
attr(randomized_response_template,"tips") <-
  c(
    N = "Size of sample",
    prob_forced_yes = "Probability of forced 'yes' response",
    prevalence_rate = "True rate of sensitive trait presence in population",
    withholding_rate = "Rate at which those with sensitive trait conceal it when asked directly"
  )


