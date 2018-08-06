#' Create a randomized response design
#'
#' Produces a (forced) randomized response design that measures the share of individuals with a given trait \code{prevalence_trait} in a population of size \code{N}. Probability of forced response ("Yes") is given by \code{prob_forced_yes}, and rate at which individuals with trait lie is given by \code{withholding_rate}.
#' 
#' @details 
#' \code{randomized_response_designer} employs a specific variation of randomized response designs in which respondents are required to report a fixed answer to the sensitive question with a given probability (see Blair, Imai, and Zhou (2015) for alternative applications and estimation strategies).
#' 
#' @param N An integer. Size of sample.
#' @param prob_forced_yes A number. Probability of a forced yes.
#' @param prevalence_rate A number. Probability that individual has the sensitive trait.
#' @param withholding_rate A number. Probability that an individual with the sensitive trait hides it.
#' @return A randomized response design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment
#' @concept descriptive
#' @import DeclareDesign stats utils fabricatr estimatr randomizr
#' @export
#'
#' @examples
#' # Generate a randomized response design using default arguments:
#' randomized_response_design <- randomized_response_designer()

randomized_response_designer <- function(N = 1000,
                                         prob_forced_yes = .6,
                                         prevalence_rate = .1,
                                         withholding_rate = .5
){
  sensitive_trait <- withholder <- Y <- Z <- bias <- NULL
  if(prob_forced_yes < 0 || prob_forced_yes > 1)   stop("prob_forced_yes must be in [0,1]")
  if(prevalence_rate < 0 || prevalence_rate > 1)   stop("prevalence_rate must be in [0,1]")
  if(withholding_rate < 0 || withholding_rate > 1) stop("withholding_rate must be in [0,1]")
  {{{
    # M: Model
    population <- declare_population(
      N = N,
      sensitive_trait = draw_binary(prob = prevalence_rate, N = N),
      withholder = draw_binary(prob = sensitive_trait * withholding_rate, N = N),
      direct_answer =  sensitive_trait - withholder
    )
    potentials <- declare_potential_outcomes(
      Y_Z_Yes = 1,
      Y_Z_Truth = sensitive_trait
    )
    
    # I: Inquiry
    estimand <- declare_estimand(true_rate = mean(sensitive_trait))
    
    # D: Data Strategy
    assignment <- declare_assignment(
      prob = prob_forced_yes,
      conditions = c("Truth","Yes")
    )
    
    # A: Answer Strategy
    estimator_randomized_response <- declare_estimator(
      handler = tidy_estimator(
        function(data) with(
          data,
          data.frame(estimate = (mean(Y) - prob_forced_yes) / (1 - prob_forced_yes)))),
      estimand = estimand,
      label = "Forced Randomized Response"
    )
    estimator_direct_question <- declare_estimator(
      handler = tidy_estimator(function(data) with(
        data,
        data.frame(estimate = mean(direct_answer)))),
      estimand = estimand,
      label = "Direct Question"
    )
    
    # Design
    randomized_response_design <- population + assignment + potentials +
      estimand + declare_reveal(Y, Z) +
      estimator_randomized_response + estimator_direct_question
    
    randomized_response_design <- set_diagnosands(
      design = randomized_response_design,
      diagnosands = declare_diagnosands(select = bias)
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
<p> A forced randomized response design that measures the share of individuals with a given trait (whose value is defined by <code>prevalence_trait</code>) in a population of size <code>N</code>. Probability of forced response ('Yes') is given by <code>prob_forced_yes</code>, and rate at which individuals with trait lie is defined by <code>withholding_rate</code>.
"

