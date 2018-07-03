#' Create a simple factorial design
#'
#' This designer builds a two by two factorial design in which asignments to each factor are independent of each other
#' If you want a factorial with non independent assignments use the multi arm designer. 
#' 
#' Three types of estimand are declared: weighted averages of the average treatment effects of each treatment over the two conditions of the other treatment and the difference in treatment effects of each over conditions of the other.
#' 
#' Treatment A is assigned first and then Treatment B within blocks defined by treatment A. Thus eg if there are 6 units in a block
#' 3 are guaranteed to receive treatment A but the number receiving treatment B is stochastic.
#' 
#' \href{/library/articles/simple_factorial_arm.html}{Check out the vignette here.}
#' 
#' Note: Units are assigned to treatment using complete random assignment. Potential outcomes follow a normal distribution.
#' @param N An integer. Sample size.
#' @param prob_A A number within the interval [0,1]. Probability of assigment to treatment A.
#' @param prob_B A number within the interval [0,1]. Probability of assigment to treatment B.
#' @param w_A Weight placed on A=1 condition in definition of "average effect of B" estimand.
#' @param w_B Weight placed on B=1 condition in definition of "average effect of A" estimand.
#' @param outcome_means A 4-vector. Average outcome in each A,B condition, in order AB = 00, 01, 10, 11.  
#' @param outcome_sds A non-negative 4-vector.  Standard deviation in each condition, in order AB = 00, 01, 10, 11.
#' @return A function that returns a design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment blocks factorial
#' @export
#'
#' @examples
#' design <- simple_factorial_designer(outcome_means = c(0,0,0,1))
#' df <- draw_data(design)
#' with(df, table(A, B, blocks))
#' get_estimates(design)
#' get_design_code(design)
#' 
#' # A design that biased for the specified estimands:
#' design <- simple_factorial_designer(outcome_means = c(0,0,0,1), prob_A = .8, prob_B = .2)
#' diagnose_design(design)
#' 
#' # A design wtih estimands that "match" the assignment:
#' design <- simple_factorial_designer(outcome_means = c(0,0,0,1), prob_A = .8, prob_B = .2, w_A = .8, w_B = .2)
#' diagnose_design(design)
#' 
#' # Compare power with and without interactions, given same average effects in each arm
#' designs <- redesign(simple_factorial_designer(), outcome_means = list(c(0,0,0,1), c(0,.5,.5,1)))
#' diagnose_design(designs)
#' 
#' 
simple_factorial_designer <- function(
  N = 100,
  prob_A = .5,
  prob_B = .5,
  w_A = .5, 
  w_B = .5, 
  outcome_sds = rep(1,4),
  outcome_means = rep(0,4)
){

  if((w_A<0) | (w_B <0) | (w_A >1) | (w_B > 1) )      stop("w_A and w_B must be in 0,1")
  if(max(outcome_sds < 0) )      stop("sd must be non-negative")
  if(max(c(prob_A, prob_B) < 0)) stop("prob must be non-negative")
  if(max(c(prob_A, prob_B) >1))  stop("prob must not exceed 1")
  {{{

    # Model ----------------------------------------------------------------------
    population <- declare_population(N)

    potentials <- declare_potential_outcomes(
      Y_A_0_B_0 = outcome_means[1] + rnorm(N, sd = outcome_sds[1]),  
      Y_A_0_B_1 = outcome_means[2] + rnorm(N, sd = outcome_sds[2]),  
      Y_A_1_B_0 = outcome_means[3] + rnorm(N, sd = outcome_sds[3]),
      Y_A_1_B_1 = outcome_means[4] + rnorm(N, sd = outcome_sds[4]))


    # Inquiry ----------------------------------------------------------------------
    estimand_1 <- declare_estimand(
      ate_A = w_B*mean(Y_A_1_B_1 - Y_A_0_B_1) + (1-w_B)*mean(Y_A_1_B_0 - Y_A_0_B_0))

    estimand_2 <- declare_estimand(
      ate_B = w_A*mean(Y_A_1_B_1 - Y_A_1_B_0) + (1-w_A)*mean(Y_A_0_B_1 - Y_A_0_B_0))
    
    estimand_3 <- declare_estimand(
      interaction = mean((Y_A_1_B_1 - Y_A_1_B_0) - (Y_A_0_B_1 - Y_A_0_B_0)))
    
    # Data Strategy ----------------------------------------------------------------
    
    # Factorial assignments
    assign_A <- declare_assignment(prob = prob_A, assignment_variable = A)
    assign_B <- declare_assignment(prob = prob_B, assignment_variable = B)
    reveal   <- declare_reveal(Y_variables = Y, assignment_variables = c(A,B))
    
    # Answer Strategy --------------------------------------------------------------
    estimator_1 <- declare_estimator(Y ~ A + B,
                                     model = lm_robust,
                                     coefficients = c("A", "B"),
                                     estimand = c("ate_A", "ate_B"), label = "No_Interaction")
    estimator_2 <- declare_estimator(Y ~ A + B + A:B,
                                     model = lm_robust,
                                     coefficients = "A:B", 
                                     estimand = "interaction", label = "Interaction")

    # Design -----------------------------------------------------------------------
    simple_factorial_design <- population + potentials + 
                               estimand_1 + estimand_2 + estimand_3 +
                               assign_A + assign_B + reveal + 
                               estimator_1 + estimator_2
    
  }}}
  
  attr(simple_factorial_design, "code") <- 
    construct_design_code(simple_factorial_designer, match.call.defaults())
  
  simple_factorial_design
  }


attr(simple_factorial_designer, "shiny_arguments") <- list(N = c(16, 32, 64), w_A = c(0, .5)) 

attr(simple_factorial_designer, "tips") <-
  list(
    N = "Sample size",
    w_A = "Weight on B=1 condition for effect of A estimand"
  )

attr(simple_factorial_designer, "description") <- "
<p> A simple factorial design of sample size <code>N</code>.
"
