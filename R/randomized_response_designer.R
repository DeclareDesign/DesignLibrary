#' Create a randomized response design
#'
#' Produces a (forced) randomized response design that measures the share of individuals with a given trait \code{prevalence_trait} in a population of size \code{N}. Probability of forced response ("Yes") is given by \code{prob_forced_yes}, and rate at which individuals with trait lie is given by \code{withholding_rate}.
#' 
#' @details 
#' \code{randomized_response_designer} employs a specific variation of randomized response designs in which respondents are required to report a args_to_fix answer to the sensitive question with a given probability (see Blair, Imai, and Zhou (2015) for alternative applications and estimation strategies).
#' 
#' See \href{https://declaredesign.org/library/articles/randomized_response.html}{vignette online}.
#' 
#' @param N An integer. Size of sample.
#' @param prob_forced_yes A number in [0,1]. Probability of a forced yes.
#' @param prevalence_rate A number in [0,1]. Probability that individual has the sensitive trait.
#' @param withholding_rate A number in [0,1]. Probability that an individual with the sensitive trait hides it.
#' @param args_to_fix A character vector. Names of arguments to be args_to_fix in design.
#' @return A randomized response design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment
#' @concept descriptive
#' @importFrom DeclareDesign declare_assignment declare_diagnosands declare_estimand declare_estimator declare_population declare_potential_outcomes declare_reveal set_diagnosands tidy_estimator
#' @importFrom fabricatr fabricate draw_binary
#' @importFrom randomizr conduct_ra 
#' @export
#' @examples
#' # Generate a randomized response design using default arguments:
#' randomized_response_design <- randomized_response_designer()

randomized_response_designer <- function(N = 1000,
                                         prob_forced_yes = .6,
                                         prevalence_rate = .1,
                                         withholding_rate = .5,
                                         args_to_fix = NULL
){
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
    
    potential_outcomes <- declare_potential_outcomes(
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
    randomized_response_design <- population + assignment + potential_outcomes +
      estimand + declare_reveal(Y, Z) +
      estimator_randomized_response + estimator_direct_question
    
    randomized_response_design <- set_diagnosands(
      randomized_response_design,
      declare_diagnosands(select = bias)
    )
    
  }}}
  attr(randomized_response_design, "code") <- 
    construct_design_code(randomized_response_designer, args_to_fix = args_to_fix, match.call.defaults())
  randomized_response_design
}
attr(randomized_response_designer,"definitions") <- data.frame(
  names = c("N", "prob_forced_yes", "prevalence_rate", "withholding_rate","args_to_fix"),
  tips  = c("Size of sample",
            "Probability of forced 'yes' response",
            "True rate of sensitive trait presence in population",
            "Rate at which those with sensitive trait conceal it when asked directly",
            "Names of arguments to be args_to_fix"),
  class = c("integer", rep("numeric",3), "character"), 
  vector = c(FALSE, FALSE, FALSE, FALSE, TRUE),
  min   = c(1, 0, 0, 0, NA),
  max   = c(Inf,1, 1, 1, NA),
  inspector_min = c(100, 0, 0, 0, NA),
  inspector_step = c(50, rep(.2, 3), NA),
  stringsAsFactors = FALSE
)
attr(randomized_response_designer,"shiny_arguments") <-
  list(
    N = c(100, 150, 200, 250),
    prob_forced_yes = c(.6, .9),
    prevalence_rate = c(seq(.1, .9, .1)),
    withholding_rate = c(seq(.1, .9, .1))
  )
attr(randomized_response_designer,"description") <- "
<p> A forced randomized response design that measures the share of individuals 
with a given trait (<code>prevalence_trait</code>) in a population of size <code>N</code>. Probability of forced response ('Yes') is given by <code>prob_forced_yes</code>, and rate at which individuals with trait lie is defined by <code>withholding_rate</code>.
"

