#' Create a design for mediation analysis
#'
#' A mediation analysis design that examines the effect of treatment (Z) on mediator (M) and the effect of mediator (M) on outcome (Y) (given Z=0) 
#' as well as direct effect of treatment (Z) on outcome (Y) (given M=0). Analysis is implemented using an interacted regression model. 
#' Note this model is not guaranteed to be unbiased despire randomization of Z because of possible violations of sequential ignorability.
#'
#' @param N An integer. Size of sample.
#' @param a A number. Parameter governing effect of treatment (Z) on mediatior (M).
#' @param b A number. Effect of mediatior (M) on outcome (Y) when Z=0.
#' @param c A number. Interaction between mediatior (M) and (Z) for outcome (Y).
#' @param d A number. Direct effect of treatment (Z) on outcome (Y), when M = 0.
#' @param rho A number in [0,1]. Correlation between mediator (M) and outcome (Y) error terms. Non zero correlation implies a violation of sequential ignorability.
#' @return A mediation analysis design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment
#' @concept mediation
#' @import DeclareDesign stats utils fabricatr estimatr randomizr
#' @export
#' @examples
#' # Generate a mediation analysis design using default arguments:
#' mediation_analysis_design_1 <- mediation_analysis_designer()
#' get_estimands(mediation_analysis_design_1)
#' # A design with bias due to violation of sequential ignorability:
#' mediation_analysis_design_2 <- mediation_analysis_designer(rho = .5)
#' get_estimands(mediation_analysis_design_2)
#'
mediation_analysis_designer <- function(N = 100, 
                                        a = .5, b = .5, c = 0, d = .5, 
                                        rho = 0)
{
  e1 <- M_Z_1 <-M <- Z <- Y <- M_Z_0 <- Y_M_1_Z_0 <-  Y_M_0_Z_0 <- Y_M_1_Z_1 <- Y_M_0_Z_1 <-  NULL
  if(rho < -1 | rho > 1) stop("rho must be in [-1, 1]")
  {{{
    # M: Model
    population <- declare_population(
      N = N, 
      e1 = rnorm(N),
      e2 = rnorm(n = N, mean = rho * e1, sd = 1 - rho^2)
    )
    potentials_M <- declare_potential_outcomes(M ~ 1*(a * Z + e1 > 0))
    potentials_Y <- declare_potential_outcomes(Y ~ d * Z + b * M + c * M * Z + e2,
                                               conditions = list(M = 0:1, Z = 0:1))
    pots_Y_nat_0 <- declare_potential_outcomes(
                      Y_nat0_Y_Z_0 = b * M_Z_0 + e2,
                      Y_nat0_Y_Z_1 = d + b * M_Z_0 + c * M_Z_0 + e2)
    pots_Y_nat_1 <- declare_potential_outcomes(
                      Y_nat1_Y_Z_0 = b * M_Z_1 + e2,
                      Y_nat1_Y_Z_1 = d + b * M_Z_1 + c * M_Z_0 + e2)
    
    # I: Inquiry
    estimands <- declare_estimands(FirstStage = mean(M_Z_1 - M_Z_0), 
                                   Indirect_0 = mean(Y_M_1_Z_0 - Y_M_0_Z_0),
                                   Indirect_1 = mean(Y_M_1_Z_1 - Y_M_0_Z_1),
                                   Controlled_Direct_0  = mean(Y_M_0_Z_1 - Y_M_0_Z_0),
                                   Controlled_Direct_1  = mean(Y_M_1_Z_1 - Y_M_1_Z_0),
                                   Natural_Direct_0 = mean(Y_nat0_Y_Z_1 - Y_nat0_Y_Z_0),
                                   Natural_Direct_1 = mean(Y_nat1_Y_Z_1 - Y_nat1_Y_Z_0))

    # D: Data strategy 1
    assignment <- declare_assignment(prob = 0.5)
    reveal_M   <- declare_reveal(M, Z)
    reveal_Y   <- declare_reveal(Y, assignment_variable = c("M","Z"))
    
    # A: Answer Strategy
    mediator_regression <- declare_estimator(
      M ~ Z,
      model = lm_robust,
      estimand = "FirstStage",
      label = "Mediator regression"
    )
    outcome_regression <- declare_estimator(
      Y ~ M*Z,
      model = lm_robust,
      term = c("M","Z"),
      estimand = c("Indirect_0", "Controlled_Direct_0"),
      label = "Outcome regression"
    )
    
    # Design
    mediation_analysis_design <- population + potentials_M + potentials_Y +
      estimands + assignment + reveal_M + reveal_Y + 
      mediator_regression + outcome_regression
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
    effect of treatment (Z) on mediator (M) governed by <code>a</code>, 
    effect of mediator (M) on outcome (Y) (when Z = 0) equal to <code>b</code>, 
    and direct effect of treatment (Z) on outcome (Y) (when M = 0) equal to <code>d</code>. 
    Possible interaction between M and Z for Y given by c.
<p> Error terms on mediator (M) and outcome (Y) correlated by <code>rho</code>.
"



