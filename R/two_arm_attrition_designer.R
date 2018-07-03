#' Create design with risk of attrition or post treatment conditioning
#'
#' This designer creates a two arm design but where a researcher is interested in an estimand that is conditional on a post treatment outcome 
#' (the effect on Y given R) or has  access to conditional data (Y given R)
#' 
#' 
#' @param N Integer. Size of sample
#' @param b_R Coefficient relating treatment to responses
#' @param b_Y Coefficient relating treatment to outcome
#' @return A post-treatment design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team} 
#' @concept post-treatment
#' @export
#' @examples
#' # To make a design using default arguments:
#' two_arm_attrition_design <- two_arm_attrition_designer()
#' diagnose_design(two_arm_attrition_design)
#' # Attrition can produce bias even when not associated with treatment
#' diagnose_design(redesign(two_arm_attrition_design, b_R = 0:1))


two_arm_attrition_designer <- function(N = 100, 
                                       b_R = 2, 
                                       b_Y = 2
){
  {{{
    # M: Model
    population   <- declare_population(N = N, u = rnorm(N), e = rnorm(N))
    potentials_R <- declare_potential_outcomes(R ~ (b_R*Z > u))
    potentials_Y <- declare_potential_outcomes(Y ~ (b_Y*Z > u + e))
    
    # I: Inquiry
    estimand_1 <- declare_estimand(mean(R_Z_1 - R_Z_0), label = "ATE on R")
    estimand_2 <- declare_estimand(mean(Y_Z_1 - Y_Z_0), label = "ATE on Y")
    estimand_3 <- declare_estimand(mean((Y_Z_1 - Y_Z_0)[R==1]), 
                                   label = "ATE on Y (Given R)")
    
    # D: Data Strategy
    assignment <- declare_assignment(prob = 0.5)
    reveal     <- declare_reveal(outcome_variables = c("R", "Y")) 
    observed   <- declare_step(Y_obs = ifelse(R, Y, NA), handler = fabricate)    
    
    # A: Answer Strategy
    estimator_1 <-
      declare_estimator(R ~ Z, coefficients = "Z", 
                        estimand = estimand_1, label = "DIM on R")
    estimator_2 <-
      declare_estimator(Y_obs ~ Z, coefficients = "Z", 
                        estimand = c(estimand_2, estimand_3), label = "DIM on Y_obs")
    estimator_3 <-
      declare_estimator(Y ~ Z, coefficients = "Z", 
                        estimand = c(estimand_2, estimand_3), label = "DIM on Y")
    
    # Design
    two_arm_attrition_design <- population + potentials_R +  potentials_Y +
      assignment  + reveal + observed +
      estimand_1  + estimand_2  + estimand_3 +
      estimator_1 + estimator_2 + estimator_3
  }}}
  
  attr(two_arm_attrition_design, "code") <- 
    construct_design_code(two_arm_attrition_designer, match.call.defaults())
  
  two_arm_attrition_design
}

attr(two_arm_attrition_designer, "tips") <- c(N = "Size of sample", b_R = "How reporting is related to treatment")
attr(two_arm_attrition_designer, "shiny_arguments") <- list(N = c(100, 500), b_R = 0:2)
attr(two_arm_attrition_designer, "description") <- "<p> A design in which an outcome (Y) is observed conditional on a post-treatment variable (R).<p>"

