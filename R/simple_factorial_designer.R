#' Create a simple two arm design
#'
#' This designer builds a two by two factorial design in which asignments to each factor are independent of each other
#' If you want a factorial with non independent assignments use the multi arm designer. 
#' 
#' Treatment effects can be specified either by providing a four element vector  \code{control_mean} and \code{treatment_mean}
#' or by specifying a three element vector \code{ate}.
#' 
#' \href{/library/articles/simple_factorial_arm.html}{Check out the vignette here.}
#' 
#' Note: Units are assigned to treatment using complete random assignment. Potential outcomes follow a normal distribution.
#' @param N An integer. Sample size.
#' @param prob A number within the interval [0,1]. Probability of assigment to treatment.
#' @param control_mean A number. Average outcome in control.
#' @param control_sd A positive number. Standard deviation in control.
#' @param ate A number. Average treatment effect.
#' @param out_means A number. Average outcome in treatment. 
#' @param out_sds  A non-negative number. Standard deviation in treatment. 
#' @param rho A number within the interval [-1,1]. Correlation between treatment and control outcomes.
#' @return A function that returns a design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment
#' @export
#'
#' @examples
#' simple_two_arm_design <- simple_two_arm_designer()


simple_factorial_designer <- function(N = NULL,
                                      n_blocks = 2,
                                      n_per_block = NULL,
                                      prob_A = .5,
                                      prob_B = .5,
                                      control_mean = 0,
                                      outcome_means = c(0,1,1,2)/4,
                                      outcome_sd = c(1,1,1,1)
){

  if(is.null(N) & is.null(n_per_block)) stop("Please provide either N or n_per_block")
  if(is.null(n_per_block))  if(N/n_blocks > floor(N/n_blocks)) stop("N is not divisible by n_blocks")
  if(!is.null(n_per_block) & !is.null(N)) warning("N to be determined by n_blocks and n_per_block")
  if(is.null(n_per_block)) n_per_block <- N/n_blocks
  if(length(outcome_sd)==1) outcome_sd <- rep(outcome_sd, 4)
  
  if(max(outcome_sd < 0) )        stop("sd must be non-negative")
  if(max(c(prob_A, prob_B) < 0))  stop("prob must be non-negative")
  if(max(c(prob_A, prob_B) >1)) stop("prob must not exceed 1")
  {{{

    # Model ----------------------------------------------------------------------
    population <- declare_population(
      blocks = add_level(
        N = n_blocks, 
        block_shock = rnorm(N)),
      subjects  = fabricatr::add_level(
        N = n_per_block, 
        shock_00 = rnorm(N)*outcome_sd[1] + block_shock,
        shock_01 = rnorm(N)*outcome_sd[2] + block_shock,
        shock_10 = rnorm(N)*outcome_sd[3] + block_shock,
        shock_11 = rnorm(N)*outcome_sd[4] + block_shock)
    )
    
    potentials <- declare_potential_outcomes(
      Y_A_0_B_0 = outcome_means[1] + shock_00,  Y_A_1_B_0 = outcome_means[3] + shock_10,
      Y_A_0_B_1 = outcome_means[2] + shock_01,  Y_A_1_B_1 = outcome_means[4] + shock_11)
    
    # Inquiry ----------------------------------------------------------------------
    estimand_1 <- declare_estimand(
      ate_A = mean((Y_A_1_B_1 - Y_A_0_B_1) + (Y_A_1_B_0 - Y_A_0_B_0))/2)

    estimand_2 <- declare_estimand(
      ate_B = mean((Y_A_1_B_1 - Y_A_1_B_0) + (Y_A_0_B_1 - Y_A_0_B_0))/2)
    
    estimand_3 <- declare_estimand(
      interaction = mean((Y_A_1_B_1 - Y_A_1_B_0) - (Y_A_0_B_1 - Y_A_0_B_0)))
    
    # Data Strategy ----------------------------------------------------------------
    
    # Factorial assignments
    assign_A <- declare_assignment(prob = prob_A, 
      blocks = blocks, assignment_variable = A)
    assign_B <- declare_assignment(prob = prob_B,
      blocks = paste0(A, blocks), assignment_variable = B)
    
    reveal <- declare_reveal(outcome_variables = Y, assignment_variables = c(A,B))
    
    # Answer Strategy --------------------------------------------------------------
    estimator_1 <- declare_estimator(Y ~ A,
                                    fixed_effects = ~ blocks,
                                    model = lm_robust,
                                    estimand = "ate_A", label = "est_A")
    estimator_2 <- declare_estimator(Y ~ B,
                                     fixed_effects = ~ blocks,
                                     model = lm_robust,
                                     estimand = "ate_B", label = "est_B")
    estimator_3 <- declare_estimator(Y ~ A + B + A:B,
                                     fixed_effects = ~ blocks,
                                     model = lm_robust,
                                     coefficients = "A:B", 
                                     estimand = "interaction", label = "interaction")

    # Design -----------------------------------------------------------------------
    simple_factorial_design <- population + potentials + 
                               estimand_1 + estimand_2 + estimand_3 +
                               assign_A + assign_B + reveal + 
                               estimator_1 + estimator_2 + estimator_3
    
  }}}
  
  attr(simple_factorial_design, "code") <- 
    construct_design_code(simple_factorial_designer, match.call.defaults())
  
  simple_factorial_design
}

attr(simple_factorial_design, "shiny_arguments") <- list(N = c(10, 20, 50)) 

attr(simple_factorial_design, "tips") <-
  list(
    N = "Sample size"
  )

attr(simple_factorial_design, "description") <- "
<p> A simple factorial design of sample size <code>N</code>.
"


