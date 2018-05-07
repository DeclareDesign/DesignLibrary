#' Create a simple two arm design
#' @param N Number of units
#' @param code If TRUE designer returns the code of a design 
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
