#' Create design with risk of attrition or post treatment conditioning
#'
#' Creates a two arm design with application for when estimand of interest is conditional on a post treatment outcome 
#' (the effect on Y given R) or data is conditionally observed (Y given R). See `Details` for more information on the data generating process.
#' 
#' @details 
#' The data generating process is of the form: 
#' 
#'     \code{R ~ (a_R + b_R*Z > u_R)}
#'     
#'     \code{Y ~ (a_Y + b_Y*Z > u_Y)}
#'     
#'     where \code{u_R} and \code{u_Y} are joint normally distributed with correlation \code{rho}.
#' 
#' 
#' @param N An integer. Size of sample.
#' @param a_R A number. Constant in equation relating treatment to responses.
#' @param b_R A number. Slope coefficient in equation relating treatment to responses.
#' @param a_Y A number. Constant in equation relating treatment to outcome.
#' @param b_Y A number. Slope coefficient in equation relating treatment to outcome.
#' @param rho A number in [0,1]. Correlation between shocks in equations for R and Y.
#' @return A post-treatment design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team} 
#' @concept post-treatment
#' @import DeclareDesign stats utils fabricatr estimatr randomizr
#' @export
#' @examples
#' # To make a design using default argument (missing completely at random):
#' two_arm_attrition_design <- two_arm_attrition_designer()
#' \dontrun{
#' diagnose_design(two_arm_attrition_design)
#' }
#' # Attrition can produce bias even for unconditional ATE even when not
#' # associated with treatment
#' \dontrun{
#' diagnose_design(two_arm_attrition_designer(b_R = 0, rho = .3))
#' }
#' # Here the linear estimate using R=1 data is unbiased for
#' # "ATE on Y (Given R)" with b_R = 0 but not when  b_R = 1   
#' \dontrun{
#' diagnose_design(redesign(two_arm_attrition_design, b_R = 0:1, rho = .2))
#' }


two_arm_attrition_designer <- function(N = 100,
                                       a_R = 0,
                                       b_R = 1,
                                       a_Y = 0,
                                       b_Y = 1,
                                       rho = 0
){
  u_R <- R_Z_1 <- R_Z_0 <- Y_Z_0 <- Y_Z_1 <- R <- Y <- NULL
  if(rho < 0 || rho > 1) stop("rho must be in [0,1]")
  {{{
    # M: Model
    population   <- declare_population(N   = N, 
                                       u_R = rnorm(N), 
                                       u_Y = rnorm(N, mean = rho * u_R, 
                                                   sd = sqrt(1 - rho^2)))
    potentials_R <- declare_potential_outcomes(R ~ (a_R + b_R*Z > u_R))
    potentials_Y <- declare_potential_outcomes(Y ~ (a_Y + b_Y*Z > u_Y))
    
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
      declare_estimator(R ~ Z, term = "Z", 
                        estimand = estimand_1, label = "DIM on R")
    estimator_2 <-
      declare_estimator(Y_obs ~ Z, term = "Z", 
                        estimand = c(estimand_2, estimand_3), label = "DIM on Y_obs")
    estimator_3 <-
      declare_estimator(Y ~ Z, term = "Z", 
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

attr(two_arm_attrition_designer, "tips") <- c(N = "Size of sample", b_R = "How reporting is related to treatment", rho = "Correlation between reporting error term and outcome error term")
attr(two_arm_attrition_designer, "shiny_arguments") <- list(N = c(100, 500), b_R = 0:2, rho = c(0,1))
attr(two_arm_attrition_designer, "description") <- "<p> A design in which an outcome (Y) is observed conditional on a post-treatment variable (R).<p>"

