#' Create a crossover design
#'
#' This designer produces designs that have two treatments, A and B, 
#' which each correspond to their own outcomes, YA and YB. 
#' The basic premise of the design is that treatment A does not affect outcome YB, 
#' and that treatment B does not affect outome YA. Using the crossover parameter
#' researchers can assess robustness of the design to violations of this assumption.
#' The \href{/library/articles/crossover.html}{vignette} shows that adding a 
#' SUR estimator to the design can greatly increase efficiency.
#'
#' @param N An integer. Size of sample.
#' @param a A number. Treatment effect of interest
#' @param b A number. Treatment effect of crossed randomization
#' @param crossover A number. Size of crossover effect
#' @param rho A number in [0,1]. Correlation in errors of outcomes A and B
#' @return A crossover design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment
#' @concept multiarm
#' @export
#' @examples
#' # To make a design using default arguments:
#' crossover_design <- crossover_designer()
#'

crossover_designer <- function(N = 100,
                               a = .5,
                               b = .5,
                               crossover = .1, 
                               rho = .2) 
{
  {{{
    population <- declare_population(
      N = N, 
      u_a = rnorm(N),
      u_b = rnorm(n = N, mean = rho * u_a, sd = sqrt(1 - rho^2))
    )
    potential_outcomes_A <- declare_potential_outcomes(
      YA_Z_T1 = u_a,
      YA_Z_T2 = a + u_a,
      YA_Z_T3 = u_a + crossover * (b + u_b),
      YA_Z_T4 = a + u_a + crossover * (b + u_b)
    )
    potential_outcomes_B <- declare_potential_outcomes(
      YB_Z_T1 = u_b,
      YB_Z_T2 = u_b + crossover * (a + u_a),
      YB_Z_T3 = b + u_b,
      YB_Z_T4 = b + u_b + crossover * (a + u_a)
    )
    estimand <- declare_estimand(a = mean(YA_Z_T2 - YA_Z_T1))
    assignment <- declare_assignment(num_arms = 4)
    get_AB <- declare_step(
      A = as.numeric(Z %in% c("T2", "T4")),
      B = as.numeric(Z %in% c("T3", "T4")),
      handler = fabricate)
    reveal_YA <- declare_reveal(YA, Z) 
    reveal_YB <-   declare_reveal(YB, Z) 
    estimator_direct <- declare_estimator(YA ~ A,
                                          model = lm_robust,
                                          coefficients = "A",
                                          estimand = estimand,
                                          label = "Direct estimator")
    estimator_sat <- declare_estimator(YA ~ A + B,
                                       model = lm_robust,
                                       coefficients = "A",
                                       estimand = estimand,
                                       label = "Saturated estimator")
    crossover_design <- 
      population +
      potential_outcomes_A +
      potential_outcomes_B +
      estimand +
      assignment +
      get_AB +
      reveal_YA + 
      reveal_YB +
      estimator_direct + 
      estimator_sat 
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







