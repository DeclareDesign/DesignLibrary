#' Create a simple two arm design
#'
#' This designer builds a design with one treatment and one control arm.
#' Treatment effects can be specified either by providing \code{control_mean} and \code{treatment_mean}
#' or by specifying an \code{ate}.
#' 
#' Note: Units are assigned to treatment using complete random assignment. Potential outcomes follow a normal distribution.
#' @param N An integer. Sample size.
#' @param prob A number within the interval [0,1]. Probability of assigment to treatment.
#' @param control_mean A number. Average outcome in control.
#' @param control_sd A positive number. Standard deviation in control.
#' @param ate A number. Average treatment effect.
#' @param treatment_mean A number. Average outcome in treatment. 
#' @param treatment_sd  A non-negative number. Standard deviation in treatment. 
#' @param rho. A number within the interval [-1,1]. Correlation between treatment and control outcomes.
#' @return A function that returns a design.
#' @author  DeclareDesign Team \url{https://declaredesign.org/}
#' @export
#' @examples
#' simple_two_arm_design <- simple_two_arm_designer()


simple_two_arm_designer <- function(N = 100,
                                    prob = .5,
                                    control_mean = 0,
                                    control_sd = 1,
                                    ate = 1,
                                    treatment_mean = control_mean + ate,
                                    treatment_sd = control_sd,
                                    rho = 1
){
  if(control_sd < 0 ) stop("control_sd must be non-negative")
  if(prob < 0 | prob > 1) stop("prob must be in [0,1]")
  if( rho < -1 | rho > 1) stop("rho must be in [-1,1]")
  {{{
    # M: Model
    pop <- declare_population(
      N = N,
      Z0 = rnorm(N, mean = control_mean, sd = control_sd),
      Z1 = correlate(given = Z0, rho = rho, rnorm, mean = treatment_mean, sd = treatment_sd)
    )
    potential_outcomes <- declare_potential_outcomes(Y ~ (1-Z) * Z0 + Z * Z1)
    
    # I: Inquiry
    estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
    
    # D: Data
    assignment <- declare_assignment(prob = prob)
    
    # A: Analysis
    estimator <- declare_estimator(Y ~ Z, estimand = estimand)
    
    # Design
    simple_two_arm_design <- pop / potential_outcomes / estimand / assignment / declare_reveal() / estimator
  }}}
  
  attr(simple_two_arm_design, "code") <- 
    construct_design_code(simple_two_arm_designer, match.call.defaults())
  
  simple_two_arm_design
}

attr(simple_two_arm_designer, "shiny_arguments") <- list(N = c(10, 20, 50), ate = c(0, .5)) 

attr(simple_two_arm_designer, "tips") <-
  list(
    N = "Number of blocks",
    ate = "The average treatment effect"
  )

attr(simple_two_arm_designer, "description") <- "
<p> A simple two arm design of sample size <code>N</code> and with constant average treatment effect equal to <code>ate</code>.
"




#' A simple two arm design
#'
#' Default design created with  \code{\link{simple_two_arm_designer}}
#' 
#' @seealso \code{\link{simple_two_arm_designer}} 
#' @format A design object 
"simple_two_arm_design"




