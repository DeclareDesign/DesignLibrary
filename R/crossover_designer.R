#' Create a crossover design
#'
#' This designer produces designs that have two treatments, A and B, 
#' each with a corresponding outcomes of interest, YA and YB. 
#' The basic premise of the design is that treatment A does not affect outcome YB, 
#' and that treatment B does not affect outome YA. Using the crossover parameter
#' researchers can assess robustness of the design to violations of this assumption.
#'
#' @param N An integer. Size of sample.
#' @param a A number. Treatment effect of interest.
#' @param b A number. Treatment effect of crossed randomization.
#' @param crossover A number. Size of crossover effect.
#' @param rho A number in [-1,1]. Correlation in errors of outcomes A and B.
#' @return A crossover design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment
#' @concept multiarm
#' @import DeclareDesign stats utils fabricatr estimatr randomizr
#' @export
#' @examples
#' # Generate a crossover design using default arguments:
#' crossover_design <- crossover_designer()
#'
crossover_designer <- function(N = 100,
                               a = .5,
                               b = .5,
                               crossover = .2, 
                               rho = .2) 
{
  A <- YA_A_1_B_0 <- YA_A_0_B_0 <- u_a <- u_b <- YA <- Z <- YB <- YA_Z_T2 <- YA_Z_T1 <- NULL 
  
  if(rho < -1 || rho > 1) stop("rho must be in [-1,1]")
  if(N < 2) stop("N must be at least 2")
  {{{
    # M: Model
    population <- declare_population(
      N = N, 
      u_a = rnorm(N),
      u_b = rnorm(n = N, mean = rho * u_a, sd = sqrt(1 - rho^2))
    )
    potentials_A <- declare_potential_outcomes(
      YA ~ u_a + a*A + B* crossover * (b + u_b), conditions = list(A = 0:1, B = 0:1)
    )
    potentials_B <- declare_potential_outcomes(
      YB ~ u_b + b*B + A* crossover * (a + u_a), conditions = list(A = 0:1, B = 0:1)
    )

    # I: Inquiry
    estimand <- declare_estimand(a = mean(YA_A_1_B_0 - YA_A_0_B_0))
    
    # D: Data Strategy
    assignment_A <- declare_assignment(assignment_variable = "A")
    assignment_B <- declare_assignment(assignment_variable = "B", blocks = A)
    reveal_YA    <- declare_reveal(YA, assignment_variables = c("A", "B")) 
    reveal_YB    <- declare_reveal(YB, assignment_variables = c("A", "B")) 
    

    # A: Answer Strategy
    estimator_dir <- declare_estimator(YA ~ A,
                                       model = lm_robust,
                                       estimand = estimand,
                                       label = "Direct estimator")
    estimator_sat <- declare_estimator(YA ~ A,
                                       model = lm_lin,
                                       covariates = ~B,
                                       estimand = estimand,
                                       label = "Saturated estimator")
    
    # Design
    crossover_design <- population + potentials_A + potentials_B +
      estimand + assignment_A + assignment_B + reveal_YA  + reveal_YB +
      estimator_dir + estimator_sat 
  }}}
  attr(crossover_design, "code") <- 
    construct_design_code(crossover_designer, match.call.defaults())
  
  crossover_design
}
attr(crossover_designer, "shiny_arguments") <- list(
  N = c(100, 50, 1000),
  a = seq(from = .5, to = -.5, by = -.5),
  b = seq(from = .5, to = -.5, by = -.5),
  crossover = c(0,.1,.25),
  rho = c(0,.5,.8))
attr(crossover_designer,"tips") <- c(
  N = "Size of sample",
  a = "Treatment effect of interest",
  b = "Treatment effect of crossed randomization",
  crossover = "Size of crossover effect",
  rho = "Correlation in errors of outcomes A and B"
)
attr(crossover_designer,"description") <- "
<p> A crossover design with a size <code>N</code> sample. 
Constant main effect of interest equal to <code>a</code>, effect of other 
treatment on other outcome equal to <code>b</code>. 
Proportion of <code>b</code> that crosses over onto main outcome of interest is equal to <code>crossover</code>. 
Outcomes correlated by <code>rho</code>.
"