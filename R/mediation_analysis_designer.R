#' Create a design for mediation analysis
#'
#' A mediation analysis design that examines the effect of treatment (Z) on mediator (M) and the effect of mediator (M) on outcome (Y) (given Z=0) 
#' as well as direct effect of treatment (Z) on outcome (Y) (given M=0). Analysis is implemented using an interacted regression model. 
#' Note this model is not guaranteed to be unbiased despite randomization of Z because of possible violations of sequential ignorability.
#' 
#' @details 
#' 
#' See \href{https://declaredesign.org/r/designlibrary/articles/mediation_analysis.html}{vignette online}.
#' 
#' @param N An integer. Size of sample.
#' @param a A number. Parameter governing effect of treatment (Z) on mediator (M).
#' @param b A number. Effect of mediator (M) on outcome (Y) when Z = 0.
#' @param c A number. Interaction between mediator (M) and (Z) for outcome (Y).
#' @param d A number. Direct effect of treatment (Z) on outcome (Y), when M = 0.
#' @param rho A number in [-1,1]. Correlation between mediator (M) and outcome (Y) error terms. Non zero correlation implies a violation of sequential ignorability.
#' @param args_to_fix A character vector. Names of arguments to be args_to_fix in design.
#' @return A mediation analysis design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment
#' @concept mediation
#' @importFrom DeclareDesign declare_assignment declare_inquiries declare_estimator declare_population declare_potential_outcomes declare_reveal declare_step diagnose_design draw_estimands
#' @importFrom fabricatr fabricate fabricate
#' @importFrom randomizr conduct_ra 
#' @importFrom estimatr lm_robust
#' @export
#' @examples
#' # Generate a mediation analysis design using default arguments:
#' mediation_1 <- mediation_analysis_designer()
#' draw_estimands(mediation_1)
#' \dontrun{
#' diagnose_design(mediation_1, sims = 1000)
#' }
#' 
#' # A design with a violation of sequential ignorability and heterogeneous effects:
#' mediation_2 <- mediation_analysis_designer(a = 1, rho = .5, c = 1, d = .75)
#' draw_estimands(mediation_2)
#' \dontrun{
#' diagnose_design(mediation_2, sims = 1000)
#' }
#'
mediation_analysis_designer <- function(N = 200, a = 1, b = .4, c = 0, d = .5, rho = 0, args_to_fix = NULL)
{
  
  if(abs(rho) > 1) stop("rho must be in [-1, 1]")
  {{{
    # M: Model
    population <- declare_population(
      N = N, 
      e1 = rnorm(N),
      e2 = rnorm(n = N, mean = rho * e1, sd = sqrt(1 - rho^2))
    )
    
    POs_M <- declare_potential_outcomes(M ~ 1*(a * Z + e1 > 0))
    
    POs_Y <- declare_potential_outcomes(Y ~ d * Z + b * M + c * M * Z + e2,
                                        conditions = list(M = 0:1, Z = 0:1))
    
    POs_Y_nat_0 <- declare_potential_outcomes(
      Y_nat0_Z_0 = b * M_Z_0 + e2,
      Y_nat0_Z_1 = d + b * M_Z_0 + c * M_Z_0 + e2)
    
    POs_Y_nat_1 <- declare_potential_outcomes(
      Y_nat1_Z_0 = b * M_Z_1 + e2,
      Y_nat1_Z_1 = d + b * M_Z_1 + c * M_Z_1 + e2)
    
    # I: Inquiry
    estimands <- declare_inquiries(
      FirstStage = mean(M_Z_1 - M_Z_0), 
      Indirect_0 = mean(Y_M_1_Z_0 - Y_M_0_Z_0),
      Indirect_1 = mean(Y_M_1_Z_1 - Y_M_0_Z_1),
      Controlled_Direct_0 = mean(Y_M_0_Z_1 - Y_M_0_Z_0),
      Controlled_Direct_1 = mean(Y_M_1_Z_1 - Y_M_1_Z_0),
      Natural_Direct_0 = mean(Y_nat0_Z_1 - Y_nat0_Z_0),
      Natural_Direct_1 = mean(Y_nat1_Z_1 - Y_nat1_Z_0)
    )
    
    # D: Data strategy 
    assignment <- declare_assignment(Z = complete_ra(N, prob = 0.5))
    
    reveal_M <- declare_reveal(M, Z)
    
    reveal_Y <- declare_reveal(Y, assignment_variable = c("M","Z"))
    
    reveal_nat0 <- declare_reveal(Y_nat0)
    
    reveal_nat1 <- declare_reveal(Y_nat1)
    
    manipulation <- declare_step(Not_M = 1 - M, handler = fabricate)
    
    # A: Answer Strategy
    mediator_regression <- declare_estimator(
      M ~ Z,
      model = lm_robust,
      inquiry = "FirstStage",
      label = "Stage 1")
    
    stage2_1 <- declare_estimator(
      Y ~ Z * M,
      model = lm_robust,
      term = c("M"),
      inquiry = c("Indirect_0"),
      label = "Stage 2"
    )
    
    stage2_2 <- declare_estimator(
      Y ~ Z * M,
      model = lm_robust,
      term = c("Z"),
      inquiry = c("Controlled_Direct_0", "Natural_Direct_0"),
      label = "Direct_0"
    )
    
    stage2_3 <- declare_estimator(
      Y ~ Z * Not_M,
      model = lm_robust,
      term = c("Z"),
      inquiry = c("Controlled_Direct_1", "Natural_Direct_1"),
      label = "Direct_1"
    )
    # Design
    mediation_analysis_design <- population + 
      POs_M + POs_Y + POs_Y_nat_0 + POs_Y_nat_1 +
      estimands + assignment + 
      reveal_M + reveal_Y + reveal_nat0 + reveal_nat1 + manipulation +
      mediator_regression + stage2_1 + stage2_2 + stage2_3
    
  }}}
  attr(mediation_analysis_design, "code") <- 
    construct_design_code(mediation_analysis_designer, args_to_fix = args_to_fix, match.call.defaults())
  
  mediation_analysis_design
}

attr(mediation_analysis_designer,"definitions") <- data.frame(
  names = c("N",  "a",  "b",  "c",  "d",  "rho", "args_to_fix"),
  tips  = c("Size of sample",
            "Effect of treatment (Z) on mediator (M)",
            "Effect of mediator (M) on outcome (Y)",
            "Interaction between mediator (M) and (Z) for outcome (Y)",
            "Direct effect of treatment (Z) on outcome (Y)",
            "Correlation of mediator (M) and outcome (Y) error terms",
            "Names of arguments to be args_to_fix"),
  class = c("integer", rep("numeric", 5), "character"),
  vector = c(rep(FALSE, 6), TRUE),
  min = c(1, rep(-Inf, 4), -1, NA),
  max = c(1, rep(Inf, 4), 1, NA),
  inspector_min = c(100, rep(0, 4), -1, NA),
  inspector_step = c(50, 0.1, rep(.2, 3), .5, NA),
  stringsAsFactors = FALSE
)

attr(mediation_analysis_designer,"shiny_arguments") <- list(
  N = c(100, 50, 1000),
  a = seq(from = .5, to = -.5, by = -.5),
  b = seq(from = .5, to = -.5, by = -.5),
  d = seq(from = .5, to = -.5, by = -.5),
  rho = c(.2, seq(from = -1, to = 1, by = .5))
)

attr(mediation_analysis_designer,"description") <- "
<p> A mediation analysis design with sample size <code>N</code> that examines
the effect of treatment (Z) on mediator (M) and the effect of mediator (M) on 
outcome (Y) (given Z=0) as well as direct effect of treatment (Z) on outcome
(Y) (given M=0).

<p> Analysis is implemented using an interacted regression model.

<p> Error terms on mediator (M) and outcome (Y) correlated by <code>rho</code>
"


