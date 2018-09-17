#' Create a simple two arm design
#'
#' Builds a design with one treatment and one control arm.
#' Treatment effects can be specified either by providing \code{control_mean} and \code{treatment_mean}
#' or by specifying a \code{control_mean} and \code{ate}.
#' 
#' @details 
#' Units are assigned to treatment using complete random assignment. Potential outcomes are normally distributed according to the mean and sd arguments.
#' 
#' See \href{https://declaredesign.org/library/articles/simple_two_arm.html}{vignette online}.
#' 
#' @param N An integer. Sample size.
#' @param assignment_prob A number in [0,1]. Probability of assignment to treatment.
#' @param control_mean A number. Average outcome in control.
#' @param control_sd A positive number. Standard deviation in control.
#' @param ate A number. Average treatment effect.
#' @param treatment_mean A number. Average outcome in treatment. Overrides \code{ate} if both specified.
#' @param treatment_sd  A nonnegative number. Standard deviation in treatment. By default equals \code{control_sd}.
#' @param rho A number in [-1,1]. Correlation between treatment and control outcomes.
#' @return A simple two-arm design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment
#' @importFrom DeclareDesign declare_assignment declare_estimand declare_estimator declare_population declare_potential_outcomes declare_reveal
#' @importFrom fabricatr fabricate 
#' @importFrom randomizr conduct_ra 
#' @importFrom estimatr tidy
#' @importFrom stats rnorm
#' @export
#'
#' @examples
#' #Generate a simple two-arm design using default arguments
#' simple_two_arm_design <- simple_two_arm_designer()


simple_two_arm_designer <- function(N = 100,
                                    assignment_prob = .5,
                                    control_mean = 0,
                                    control_sd = 1,
                                    ate = 1,
                                    treatment_mean = control_mean + ate,
                                    treatment_sd = control_sd,
                                    rho = 1
){
  if(control_sd < 0 ) stop("control_sd must be non-negative")
  if(assignment_prob < 0 || assignment_prob > 1) stop("assignment_prob must be in [0,1]")
  if(abs(rho) > 1) stop("rho must be in [-1,1]")
  {{{
    # M: Model
    population <- declare_population(
      N = N,
      u_0 = rnorm(N),
      u_1 = rnorm(n = N, mean = rho * u_0, sd = sqrt(1 - rho^2)))
    
    potential_outcomes <- declare_potential_outcomes(
      Y ~ (1-Z) * (u_0*control_sd + control_mean) + 
          Z     * (u_1*treatment_sd + treatment_mean))
    
    # I: Inquiry
    estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
    
    # D: Data Strategy
    assignment <- declare_assignment(prob = assignment_prob)
    reveal_Y    <- declare_reveal()
    
    # A: Answer Strategy
    estimator <- declare_estimator(Y ~ Z, estimand = estimand)
    
    # Design
    simple_two_arm_design <- population + potential_outcomes + estimand + assignment + reveal_Y + estimator
  }}}
  
  attr(simple_two_arm_design, "code") <- 
    construct_design_code(designer = simple_two_arm_designer, 
                          args = match.call.defaults(), 
                          exclude_args = "ate",
                          arguments_as_values = TRUE)
  
  simple_two_arm_design
}

attr(simple_two_arm_designer, "shiny_arguments") <- list(N = c(10, 20, 50), ate = c(0, .5)) 

attr(simple_two_arm_designer, "tips") <-
  list(
    N = "Sample size",
    ate = "The average treatment effect"
  )

attr(simple_two_arm_designer, "description") <- "
<p> A simple two arm design of sample size <code>N</code> and with average treatment effect equal to <code>ate</code>.
"


