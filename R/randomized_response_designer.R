#' Create a randomized response design
#'
#' Description here
#' 
#' Key limitations: Limitations here.
#' 
#' Note: Note here.
#'
#' @param N An integer. Size of sample.
#' @param prob_forced_yes A number. Probability of a forced yes.
#' @param prevalence_rate A number. Probability that individual has the sensitive trait.
#' @param withholding_rate A number. Probability that an individual with the sensitive trait hides it.
#' @return A randomized response design.
#' @author  DeclareDesign Team \url{https://declaredesign.org/}
#' @export
#'
#' @examples
#' # To make a design using default arguments:
#' randomized_response_design <- randomized_response_designer()

randomized_response_designer <- function(
  N = 1000,
  prob_forced_yes = .6,
  prevalence_rate = .1,
  withholding_rate = .5
){
  {{{
    population <- declare_population(
      N = N,
      sensitive_trait = draw_binary(prob = prevalence_rate, N = N),
      withholder = draw_binary(prob = sensitive_trait * withholding_rate, N = N),
      direct_answer =  sensitive_trait - withholder
    )
    coin_flip <- declare_assignment(
      prob = prob_forced_yes,
      conditions = c("Truth","Yes")
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
    randomized_response_design <- declare_design(
      population,
      coin_flip,
      potential_outcomes,
      estimand,
      declare_reveal(Y,Z),
      randomized_response_estimator,
      direct_question_estimator
    )
  }}}
  attr(randomized_response_design, "code") <- 
    construct_design_code(randomized_response_designer, match.call.defaults())
  randomized_response_design
}
attr(randomized_response_designer,"tips") <-
  list(
    N = "Size of sample",
    prob_forced_yes = "Probability of forced 'yes' response",
    prevalence_rate = "True rate of sensitive trait presence in population",
    withholding_rate = "Rate at which those with sensitive trait conceal it when asked directly"
  )
attr(randomized_response_designer,"shiny_arguments") <-
  list(
    N = c(1000, 1500, 2000, 2500),
    prob_forced_yes = c(.6,seq(.1,.9,.1)),
    prevalence_rate = c(.1,seq(.05,.95,.1)),
    withholding_rate = c(.5,seq(.05,.95,.1))
  )
attr(randomized_response_designer,"description") <- "
<p> A randomized response design
"




#' A randomized response design
#'
#' Default design created with  \code{\link{randomized_response_designer}}
#' 
#' @seealso \code{\link{randomized_response_designer}} 
#' @format A design object 
"randomized_response_design"






