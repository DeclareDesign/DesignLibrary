#' Create a randomized response design
#'
#' Produces a (forced) randomized response design that measures the share of individuals with a given trait \code{prevalence_trait} in a population of size \code{N}. Probability of forced response ("Yes") is given by \code{prob_forced_yes}, and rate at which individuals with trait lie is given by \code{withholding_rate}.
#' 
#' @details 
#' \code{randomized_response_designer} employs a specific variation of randomized response designs in which respondents are required to report a fixed answer to the sensitive question with a given probability (see Blair, Imai, and Zhou (2015) for alternative applications and estimation strategies).
#' 
#' See \href{https://declaredesign.org/library/articles/randomized_response.html}{vignette online}.
#' 
#' @param N An integer. Size of sample.
#' @param prob_forced_yes A number in [0,1]. Probability of a forced yes.
#' @param prevalence_rate A number in [0,1]. Probability that individual has the sensitive trait.
#' @param withholding_rate A number in [0,1]. Probability that an individual with the sensitive trait hides it.
#' @param design_name A character vector. Name of design. This is the label of the design object returned by \code{get_design_code()}. Must be provided without spacing.
#' @param fixed A character vector. Names of arguments to be fixed in design.
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
                                         design_name = "randomized_response_design",
                                         fixed = NULL
){
  if(prob_forced_yes < 0 || prob_forced_yes > 1)   stop("prob_forced_yes must be in [0,1]")
  if(prevalence_rate < 0 || prevalence_rate > 1)   stop("prevalence_rate must be in [0,1]")
  if(withholding_rate < 0 || withholding_rate > 1) stop("withholding_rate must be in [0,1]")
  argument_names <- names(match.call.defaults(envir = parent.frame()))[-1]
  fixed_wrong <- fixed[!fixed %in% names(as.list(match.call()))]
  if(length(fixed_wrong)!=0) stop(paste0("The following arguments in `fixed` do not match a designer argument:", fixed_wrong)) 
  
  N_ <- N; prob_forced_yes_ <- prob_forced_yes; prevalence_rate_ <- prevalence_rate; withholding_rate_ <- withholding_rate
  
  if(!"N" %in% fixed) N_ <- expr(N)
  if(!"prob_forced_yes" %in% fixed) prob_forced_yes_ <- expr(prob_forced_yes)
  if(!"prevalence_rate" %in% fixed) prevalence_rate_ <- expr(prevalence_rate)
  if(!"withholding_rate" %in% fixed) withholding_rate_ <- expr(withholding_rate)
  
  population_expr <- expr(declare_population(
    N = !!N_,
    sensitive_trait = draw_binary(prob = !!prevalence_rate_, N = N),
    withholder = draw_binary(prob = sensitive_trait * !!withholding_rate_, N = N),
    direct_answer =  sensitive_trait - withholder
  ))
  
  assignment_expr <- expr(declare_assignment(
    prob = !!prob_forced_yes_,
    conditions = c("Truth","Yes")
  ))
  
  estimator_randomized_response_expr <- expr(declare_estimator(
    handler = tidy_estimator(
      function(data) with(
        data,
        data.frame(estimate = (mean(Y) - !!prob_forced_yes_) / (1 - !!prob_forced_yes_)))),
    estimand = estimand,
    label = "Forced Randomized Response"
  ))
  
  {{{
    # M: Model
    population <- eval_bare(population_expr)
    
    potential_outcomes <- declare_potential_outcomes(
      Y_Z_Yes = 1,
      Y_Z_Truth = sensitive_trait
    )
    
    # I: Inquiry
    estimand <- declare_estimand(true_rate = mean(sensitive_trait))
    
    # D: Data Strategy
    
    assignment <- eval_bare(assignment_expr)
    
    # A: Answer Strategy
    estimator_randomized_response <- eval_bare(estimator_randomized_response_expr)
    
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
      design = randomized_response_design,
      diagnosands = declare_diagnosands(select = bias)
    )
    
  }}}
  
  design_code <- construct_design_code(designer = randomized_response_designer, 
                                       args = match.call.defaults(), 
                                       exclude_args = union(c("design_name", "fixed"), fixed),
                                       arguments_as_values = TRUE)
  
  design_code <- sub_expr_text(design_code, population_expr, assignment_expr, 
                               estimator_randomized_response_expr)
  
  design_code <- gsub("randomized_response_design <-", paste0(design_name, " <-"), design_code, fixed = TRUE)
  
  attr(randomized_response_design, "code") <- design_code
  
  randomized_response_design

}
attr(randomized_response_designer,"definitions") <- data.frame(
  names = c("N", "prob_forced_yes", "prevalence_rate", "withholding_rate", "design_name", "fixed"),
  class = c("integer", rep("numeric",3), rep("character", 2)),
  min   = c(1, 0, 0, 0, NA, NA),
  max   = c(Inf,1, 1, 1, NA, NA)
)
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
    prob_forced_yes = c(.6,seq(.1,.5,.1),seq(.7,.9,.1)),
    prevalence_rate = c(.1,seq(.05,.95,.1)),
    withholding_rate = c(.5,seq(.05,.95,.1))
  )
attr(randomized_response_designer,"description") <- "
<p> A forced randomized response design that measures the share of individuals 
with a given trait (<code>prevalence_trait</code>) in a population of size <code>N</code>. Probability of forced response ('Yes') is given by <code>prob_forced_yes</code>, and rate at which individuals with trait lie is defined by <code>withholding_rate</code>.
"

