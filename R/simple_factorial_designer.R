#' Create a simple factorial design
#'
#' Builds a two-by-two factorial design in which assignments to each factor are independent of each other.
#' 
#' @details
#' Three types of estimand are declared: weighted averages of the average treatment effects of each treatment over the two conditions of the other treatment and the difference in treatment effects of each over conditions of the other.
#' 
#' Treatment A is assigned first and then Treatment B within blocks defined by treatment A. Thus eg if there are 6 units 
#' 3 are guaranteed to receive treatment A but the number receiving treatment B is stochastic.
#' 
#' Units are assigned to treatment using complete random assignment. Potential outcomes follow a normal distribution.
#' 
#' See \code{\link{multi_arm_designer}} for a factorial design with non independent assignments.
#' 
#' @param N An integer. Size of sample.
#' @param prob_A A number in [0,1]. Probability of assignment to treatment A.
#' @param prob_B A number in [0,1]. Probability of assignment to treatment B.
#' @param w_A A number. Weight placed on A=1 condition in definition of "average effect of B" estimand.
#' @param w_B A number. Weight placed on B=1 condition in definition of "average effect of A" estimand.
#' @param outcome_means A vector of length 4. Average outcome in each A,B condition, in order AB = 00, 01, 10, 11. Values overridden by mean_A0B0, mean_A0B1, mean_A1B0, if provided mean_A1B1.
#' @param mean_A0B0 A number. Mean outcome in A=0, B=0 condition.
#' @param mean_A0B1 A number. Mean outcome in A=0, B=1 condition.
#' @param mean_A1B0 A number. Mean outcome in A=1, B=0 condition.
#' @param mean_A1B1 A number. Mean outcome in A=1, B=1 condition.
#' @param outcome_sds A non-negative 4-vector.  Standard deviation in each condition, in order AB = 00, 01, 10, 11.
#' @return A two-by-two factorial design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment factorial
#' @import DeclareDesign stats utils fabricatr estimatr randomizr
#' @export
#'
#' @examples
#' design <- simple_factorial_designer(outcome_means = c(0,0,0,1))
#' 
#' # A design biased for the specified estimands:
#' design <- simple_factorial_designer(outcome_means = c(0,0,0,1), prob_A = .8, prob_B = .2)
#' \dontrun{
#' diagnose_design(design)
#' }
#' 
#' # A design with estimands that "match" the assignment:
#' design <- simple_factorial_designer(outcome_means = c(0,0,0,1), 
#'                                     prob_A = .8, prob_B = .2, 
#'                                     w_A = .8, w_B = .2)
#' \dontrun{
#' diagnose_design(design)
#' }
#' 
#' # Compare power with and without interactions, given same average effects in each arm
#' designs <- redesign(simple_factorial_designer(), 
#'                     outcome_means = list(c(0,0,0,1), c(0,.5,.5,1)))
#' \dontrun{
#' diagnose_design(designs)
#' }
#' 
simple_factorial_designer <- function(N = 100,
                                      prob_A = .5,
                                      prob_B = .5,
                                      w_A = .5, 
                                      w_B = .5, 
                                      outcome_means = rep(0,4),
                                      mean_A0B0 = outcome_means[1],
                                      mean_A0B1 = outcome_means[2],
                                      mean_A1B0 = outcome_means[3],
                                      mean_A1B1 = outcome_means[4],
                                      outcome_sds = rep(1,4)
){
  Y_A_0_B_0 <- Y_A_0_B_1 <- Y_A_1_B_0 <- Y_A_1_B_1 <- A <- B <- Y <- NULL
  if((w_A < 0) || (w_B < 0) || (w_A > 1) || (w_B > 1)) stop("w_A and w_B must be in 0,1")
  if(max(outcome_sds < 0) )      stop("sd must be non-negative")
  if(max(c(prob_A, prob_B) < 0)) stop("prob_ arguments must be non-negative")
  if(max(c(prob_A, prob_B) > 1))  stop("prob_ arguments must not exceed 1")
  {{{
    
    # M: Model
    population <- declare_population(N)
    
    potentials <- declare_potential_outcomes(
      Y_A_0_B_0 = mean_A0B0 + rnorm(N, sd = outcome_sds[1]),  
      Y_A_0_B_1 = mean_A0B1 + rnorm(N, sd = outcome_sds[2]),  
      Y_A_1_B_0 = mean_A1B0 + rnorm(N, sd = outcome_sds[3]),
      Y_A_1_B_1 = mean_A1B1 + rnorm(N, sd = outcome_sds[4]))
    
    
    # I: Inquiry
    estimand_1 <- declare_estimand(
      ate_A = w_B*mean(Y_A_1_B_1 - Y_A_0_B_1) + (1-w_B)*mean(Y_A_1_B_0 - Y_A_0_B_0))
    
    estimand_2 <- declare_estimand(
      ate_B = w_A*mean(Y_A_1_B_1 - Y_A_1_B_0) + (1-w_A)*mean(Y_A_0_B_1 - Y_A_0_B_0))
    
    estimand_3 <- declare_estimand(
      interaction = mean((Y_A_1_B_1 - Y_A_1_B_0) - (Y_A_0_B_1 - Y_A_0_B_0)))
    
    # D: Data Strategy
    
    # Factorial assignments
    assign_A <- declare_assignment(prob = prob_A, assignment_variable = A)
    assign_B <- declare_assignment(prob = prob_B, assignment_variable = B)
    reveal_Y   <- declare_reveal(Y_variables = Y, assignment_variables = c(A,B))
    
    # A: Answer Strategy
    estimator_1 <- declare_estimator(Y ~ A + B,
                                     model = lm_robust,
                                     term = c("A", "B"),
                                     estimand = c("ate_A", "ate_B"), 
                                     label = "No_Interaction")
    estimator_2 <- declare_estimator(Y ~ A + B + A:B,
                                     model = lm_robust,
                                     term = "A:B", 
                                     estimand = "interaction", 
                                     label = "Interaction")
    
    # Design
    simple_factorial_design <- population + potentials + 
      estimand_1 + estimand_2 + estimand_3 +
      assign_A + assign_B + reveal_Y + 
      estimator_1 + estimator_2
    
  }}}
  
  attr(simple_factorial_design, "code") <- 
    construct_design_code(designer = simple_factorial_designer, 
                          args = match.call.defaults(), 
                          exclude_args = "outcome_means",
                          arguments_as_values = TRUE)
  
  simple_factorial_design
}


attr(simple_factorial_designer, "shiny_arguments") <- list(
  N = c(16, 32, 64), w_A = c(0, .5), 
  mean_A0B1 = 0:1, 
  mean_A1B0 = 0:1, 
  mean_A1B1 = -1:3) 

attr(simple_factorial_designer, "tips") <-
  list(
    N = "Sample size",
    w_A = "Weight on B=1 condition for effect of A estimand",
    mean_A1B0 = "Mean outcome for A=1, B=0",
    mean_A0B1 = "Mean outcome for A=0, B=1",
    mean_A1B1 = "Mean outcome for A=1, B=1"
  )

attr(simple_factorial_designer, "description") <- "
<p> A 2x2 factorial design of sample size <code>N</code>.
"
