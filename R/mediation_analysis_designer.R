#' Create a design for mediation analysis
#'
#' A mediation analysis design that examines the effect of treatment (Z) on mediator (M) and the effect of mediator (M) on outcome (Y) (given Z=0) 
#' as well as direct effect of treatment (Z) on outcome (Y) (given M=0). Analysis is implemented using an interacted regression model. 
#' Note this model is not guaranteed to be unbiased despite randomization of Z because of possible violations of sequential ignorability.
#'
#' @param N An integer. Size of sample.
#' @param a A number. Parameter governing effect of treatment (Z) on mediator (M).
#' @param b A number. Effect of mediator (M) on outcome (Y) when Z=0.
#' @param c A number. Interaction between mediator (M) and (Z) for outcome (Y).
#' @param d A number. Direct effect of treatment (Z) on outcome (Y), when M = 0.
#' @param rho A number in [-1,1]. Correlation between mediator (M) and outcome (Y) error terms. Non zero correlation implies a violation of sequential ignorability.
#' @return A mediation analysis design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment
#' @concept mediation
#' @import DeclareDesign stats utils fabricatr estimatr randomizr
#' @export
#' @examples
#' # Generate a mediation analysis design using default arguments:
#' mediation_1 <- mediation_analysis_designer()
#' get_estimands(mediation_1)
#' \dontrun{
#' diagnose_design(mediation_1, sims = 1000)
#' }
#' 
#' # A design with a violation of sequential ignorability and heterogeneous effects:
#' mediation_2 <- mediation_analysis_designer(a = 1, rho = .5, c = 1, d = .75)
#' get_estimands(mediation_2)
#' \dontrun{
#' diagnose_design(mediation_2, sims = 1000)
#' }
#'
mediation_analysis_designer <- function(N = 200, a = 1, b = .4, c = 0, d = .5, rho = 0)
{
  e1 <- M_Z_1 <- M <- Z <- Y <- M_Z_0 <- Y_M_1_Z_0 <-  
    Y_M_0_Z_0 <- Y_M_1_Z_1 <- Y_M_0_Z_1 <-  e2 <- 
    Y_nat0_Z_1 <- Y_nat0_Z_0 <- Y_nat1_Z_1 <- Y_nat1_Z_0 <- 
    Y_nat0 <- Y_nat1 <- NULL
  
  if(abs(rho) > 1) stop("rho must be in [-1, 1]")
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
      Y_nat0_Z_0 =     b * M_Z_0             + e2,
      Y_nat0_Z_1 = d + b * M_Z_0 + c * M_Z_0 + e2)
    pots_Y_nat_1 <- declare_potential_outcomes(
      Y_nat1_Z_0 =     b * M_Z_1             + e2,
      Y_nat1_Z_1 = d + b * M_Z_1 + c * M_Z_1 + e2)
    
    # I: Inquiry
    estimands <- declare_estimands(
      FirstStage          = mean(M_Z_1      - M_Z_0), 
      Indirect_0          = mean(Y_M_1_Z_0  - Y_M_0_Z_0),
      Indirect_1          = mean(Y_M_1_Z_1  - Y_M_0_Z_1),
      Controlled_Direct_0 = mean(Y_M_0_Z_1  - Y_M_0_Z_0),
      Controlled_Direct_1 = mean(Y_M_1_Z_1  - Y_M_1_Z_0),
      Natural_Direct_0    = mean(Y_nat0_Z_1 - Y_nat0_Z_0),
      Natural_Direct_1    = mean(Y_nat1_Z_1 - Y_nat1_Z_0)
    )
    
    # D: Data strategy 
    assignment   <- declare_assignment()
    reveal_M     <- declare_reveal(M, Z)
    reveal_Y     <- declare_reveal(Y, assignment_variable = c("M","Z"))
    reveal_nat0  <- declare_reveal(Y_nat0)
    reveal_nat1  <- declare_reveal(Y_nat1)
    manipulation <- declare_step(Not_M = 1-M, handler = fabricate)
    
    # A: Answer Strategy
    mediator_regression <- declare_estimator(
      M ~ Z,
      model = lm_robust,
      estimand = "FirstStage",
      label = "Stage 1")
    stage2_1 <- declare_estimator(
      Y ~ Z * M,
      model = lm_robust,
      term = c("M"),
      estimand = c("Indirect_0"),
      label = "Stage 2"
    )
    
    
    stage2_2 <- declare_estimator(
      Y ~ Z * M,
      model = lm_robust,
      term = c("Z"),
      estimand = c("Controlled_Direct_0", "Natural_Direct_0"),
      label = "Direct_0"
    )
    
    stage2_3 <- declare_estimator(
      Y ~ Z * Not_M,
      model = lm_robust,
      term = c("Z"),
      estimand = c("Controlled_Direct_1", "Natural_Direct_1"),
      label = "Direct_1"
    )
    
    # Design
    mediation_analysis_design <- population + 
      potentials_M + potentials_Y + pots_Y_nat_0 + pots_Y_nat_1 +
      estimands + assignment + 
      reveal_M + reveal_Y + reveal_nat0 + reveal_nat1 + manipulation +
      mediator_regression + stage2_1 +  stage2_2 + stage2_3
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
  a = "Effect of treatment (Z) on mediator (M)",
  b = "Effect of mediator (M) on outcome (Y)",
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



