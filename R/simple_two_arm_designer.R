#' Create a simple two arm design
#' @param N Number of units
#' @param control_mean Scalar. Average value for Y(0).
#' @param control_sd Non negative scalar. Standard deviation for Y(0).
#' @param ate scalar. Average treatment effect.
#' @param treatment_mean Treatment mean. Defaults to control_mean + ate; if specified overrides ate 
#' @param treatment_sd Non negative scalar. Standard deviation on Y(1) potential outcomes. Defaults to standard deviation of Y(0) potential outcomes 
#' @param rho scalar in [0,1]. Correlation between Y(0) and Y(1)
#' @return a function that returns a design
#' @export
#'
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
  
  {{{
    # M: Model
    pop <- declare_population(
      N = N,
      u_0 = rnorm(N, sd = control_sd),
      u_1 = correlate(given = u_0, rho = rho, rnorm, sd = treatment_sd)
    )
    potential_outcomes <- declare_potential_outcomes(Y ~ (1-Z) * (u_0 + control_mean) + 
                                                          Z    * (u_1 + treatment_mean)
                                                     )
    
    # I: Inquiry
    estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
    
    # D: Data
    assignment <- declare_assignment(prob = prob)
    
    # A: Analysis
    estimator <- declare_estimator(Y ~ Z, estimand = estimand)
    reveal    <- declare_reveal()
    
    # Design
    simple_two_arm_design <- pop / potential_outcomes / estimand / assignment / reveal / estimator
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




