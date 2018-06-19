#' Create a crossover design
#'
#' Description here
#' 
#' Key limitations: Limitations here.
#' 
#' Note: Note here.
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
#' crossover_design <- audit_experiment_designer()
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
      noise = rnorm(N),
      u_a = rnorm(N),
      u_b = rnorm(n = N, mean = rho * u_a, sd = sqrt(1 - rho^2))
    )
    potential_outcomes_A <- declare_potential_outcomes(
      YA_Z_T1 = noise,
      YA_Z_T2 = noise + u_a + a,
      YA_Z_T3 = noise + crossover * (u_b + b),
      YA_Z_T4 = noise + u_a + a + crossover * (u_b + b)
    )
    potential_outcomes_B <- declare_potential_outcomes(
      YB_Z_T1 = noise,
      YB_Z_T2 = noise + crossover * (u_a + a),
      YB_Z_T3 = noise + u_b + b,
      YB_Z_T4 = noise + u_b + b + crossover * (u_a + a)
    )
    estimand <- declare_estimand(a = a)
    assignment <- declare_assignment(num_arms = 4)
    
    get_AB <- function(data){
      fabricate(data = data,
                A = as.numeric(Z %in% c("T2", "T4")),
                B = as.numeric(Z %in% c("T3", "T4")))
    }
    
    indicator_AB <- declare_assignment(handler = get_AB)
  
    estimator_sat <- declare_estimator(YA ~ A + B,
                                       model = lm_robust,
                                       coefficients = "A",
                                       estimand = estimand,
                                       label = "Saturated estimator")
    estimator_direct <- declare_estimator(YA ~ A,
                                          model = lm_robust,
                                          coefficients = "A",
                                          estimand = estimand,
                                          label = "Direct estimator")
    crossover_design <- 
      population +
      potential_outcomes_A +
      potential_outcomes_B +
      estimand +
      assignment +
      indicator_AB +
      declare_reveal(YA, Z) +
      declare_reveal(YB, Z) +
      estimator_sat +
      estimator_direct
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




#' A crossover design
#'
#' Default design created with  \code{\link{crossover_designer}}
#' 
#' @seealso \code{\link{crossover_designer}} 
#' @format A design object 
"crossover_design"










