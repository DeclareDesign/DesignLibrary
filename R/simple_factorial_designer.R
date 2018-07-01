#' Create a simple factorial design
#'
#' This designer builds a two by two factorial design in which asignments to each factor are independent of each other
#' If you want a factorial with non independent assignments use the multi arm designer. 
#' 
#' Designer gives possibility of including blocks for assignment and estimation.
#' Treatment A is assigned first and then Treatment B within blocks defined by treatment A. Thus eg if there are 6 units in a block
#' 3 are guaranteed to receive treatment A but the number receiving treatment B is stochastic.
#' 
#' 
#' \href{/library/articles/simple_factorial_arm.html}{Check out the vignette here.}
#' 
#' Note: Units are assigned to treatment using complete random assignment. Potential outcomes follow a normal distribution.
#' @param N An integer. Sample size.
#' @param n_blocks Integer. Number of blocks, defaults to 1.
#' @param n_per_block Integer of vector of length n_blocks. Number of units per block.
#' @param prob_A A number within the interval [0,1]. Probability of assigment to treatment A.
#' @param prob_B A number within the interval [0,1]. Probability of assigment to treatment B.
#' @param outcome_means A 4-vector. Average outcome in each condition (in order AB = 00, 01, 10, 11)
#' @param outcome_sds A non-negative 4-vector.  Standard deviation in each condition (in order AB = 00, 01, 10, 11)
#' @param block_sd A non-negative number.  Standard deviation of block shock
#' @return A function that returns a design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment blocks factorial
#' @export
#'
#' @examples
#' design <- simple_factorial_designer(N = 80, outcome_means = c(0,1,1,4)/4)
#' df <- draw_data(design)
#' with(df, table(A, B))
#' diagnose_design(design, sims = 1000)
#' get_design_code(design)


simple_factorial_designer <- function(N = 100,
                                      n_blocks = 2,
                                      n_per_block = NULL,
                                      prob_A = .5,
                                      prob_B = .5,
                                      outcome_sds = rep(1,4),
                                      pure_control = 0, 
                                      ate_A_given_B0 = 0, 
                                      ate_B_given_A0 = 0, 
                                      interaction = 0,
                                      outcome_means = pure_control + 
                                         c(0, 
                                           ate_A_given_B0,  
                                           ate_B_given_A0,  
                                           ate_B_given_A0 + ate_A_given_B0 + interaction),
                                      block_sd = 1
){

  if(is.null(N) & is.null(n_per_block)) stop("Please provide either N or n_per_block")
  if(is.null(n_per_block))  if(N/n_blocks > floor(N/n_blocks)) stop("N is not divisible by n_blocks")
  if(!is.null(n_per_block) & !is.null(N)) warning("N to be determined by n_blocks and n_per_block")
  if(is.null(n_per_block)) n_per_block <- N/n_blocks
  if(length(outcome_sds)==1) outcome_sds <- rep(outcome_sd, 4)
  
  if(max(outcome_sds < 0) )        stop("sd must be non-negative")
  if(max(c(prob_A, prob_B) < 0))  stop("prob must be non-negative")
  if(max(c(prob_A, prob_B) >1)) stop("prob must not exceed 1")
  {{{

    # Model ----------------------------------------------------------------------
    population <- declare_population(
      blocks = add_level(
        N = n_blocks, 
        block_shock = rnorm(N)*block_sd),
      subjects  = fabricatr::add_level(
        N = n_per_block, 
        shock_00 = rnorm(N)*outcome_sds[1] + block_shock,
        shock_01 = rnorm(N)*outcome_sds[2] + block_shock,
        shock_10 = rnorm(N)*outcome_sds[3] + block_shock,
        shock_11 = rnorm(N)*outcome_sds[4] + block_shock)
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


attr(simple_factorial_designer, "shiny_arguments") <- list(N = c(16, 32, 64)) 

attr(simple_factorial_designer, "tips") <-
  list(
    N = "Sample size"
  )

attr(simple_factorial_designer, "description") <- "
<p> A simple factorial design of sample size <code>N</code>.
"
