#' Create a design for mediation analysis
#'
#' A mediation analysis design that examines the effect of treatment (Z) on mediator (M) and the effect of mediator (M) on outcome (Y) as well as direct effect of treatment (Z) on outcome (Y).
#'
#' @param N An integer. Size of sample.
#' @param a A number. Effect of treatment (Z) on mediatior (M).
#' @param b A number. Effect of mediatior (M) on outcome (Y).
#' @param d A number. Direct effect of treatment (Z) on outcome (Y).
#' @param rho A number in [0,1]. Correlation of between mediator (M) and outcome (Y) error terms.
#' @return A mediation analysis design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment
#' @concept mediation
#' @export
#' @examples
#' # Generate a mediation analysis design using default arguments:
#' mediation_analysis_design <- mediation_analysis_designer()
#'


mediation_analysis_designer <- function(N = 100,
                                        a = .5,
                                        b = .5,
                                        d = .5,
                                        rho = .2)
{
  if(rho < -1 | rho > 1) stop("rho must be in [-1, 1]")
  
  {{{
    # M: Model
    population <- declare_population(
      N = N, 
      e1 = rnorm(N),
      e2 = rnorm(n = N, mean = rho * e1, sd = 1 - rho^2)
    )
    pos_M <-
      declare_potential_outcomes(M ~ a * Z + e1)
    pos_Y <-
      declare_potential_outcomes(Y ~ d * Z + b * M + e2)
    reveal_M <- declare_reveal(M, Z)
    reveal_Y <- declare_reveal(Y, Z) 
    
    # I: Inquiry
    estimand_a <- declare_estimand(a = a)
    estimand_b <- declare_estimand(b = b)
    estimand_d <- declare_estimand(d = d)
    
    # D: Data strategy
    assignment <- declare_assignment(prob = 0.5)
    
    # A: Answer Strategy
    mediator_regression <- declare_estimator(
      M ~ Z,
      model = lm_robust,
      term = "Z",
      estimand = estimand_a,
      label = "Mediator regression"
    )
    outcome_regression <- declare_estimator(
      Y ~ Z + M,
      model = lm_robust,
      term = c("M","Z"),
      estimand = c(estimand_b,estimand_d),
      label = "Outcome regression"
    )
    
    # Design
    mediation_analysis_design <-
      population +
      pos_M +
      assignment +
      reveal_M +
      pos_Y +
      estimand_a +
      estimand_b +
      estimand_d +
      reveal_Y +
      mediator_regression +
      outcome_regression
  }}}
  attr(mediation_analysis_design, "code") <- 
    construct_design_code(mediation_analysis_designer, match.call.defaults())
  
  mediation_analysis_design
}

attr(mediation_analysis_designer,"shiny_arguments") <- list(
  N = c(100, 50, 1000),
  a = seq(from = .5, to = -.5, by = -.5),
  b = seq(from = .5, to = -.5, by = -.5),
  d = seq(from = .5, to = -.5, by = -.5),
  rho = c(.2, seq(from = -1, to = 1, by = .5))
)
attr(mediation_analysis_designer,"tips") <- c(
  N = "Size of sample",
  a = "Effect of treatment (Z) on mediatior (M)",
  b = "Effect of mediatior (M) on outcome (Y)",
  d = "Direct effect of treatment (Z) on outcome (Y)",
  rho = "Correlation of mediator (M) and outcome (Y) error terms"
)
attr(mediation_analysis_designer,"description") <- "
<p> A mediation analysis design, with sample of size <code>N</code>, 
    effect of treatment (Z) on mediator (M) equal to <code>a</code>, 
    effect of mediator (M) on outcome (Y) equal to <code>b</code>, 
    and direct effect of treatment (Z) on outcome (Y) equal to <code>d</code>. 
<p> Error terms on mediator (M) and outcome (Y) correlated by <code>rho</code>.
"



