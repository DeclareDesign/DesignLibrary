#' Create a design for mediation analysis
#'
#' A mediation analysis design that examines the effect of treatment (Z) on mediator (M) and the effect of mediator (M) on outcome (Y) (given Z=0) 
#' as well as direct effect of treatment (Z) on outcome (Y) (given M=0). Analysis is implemented using an interacted regression model. 
#' Note this model is not guaranteed to be unbiased despite randomization of Z because of possible violations of sequential ignorability.
#' 
#' @details 
#' 
#' See \href{https://declaredesign.org/library/articles/mediation_analysis.html}{vignette online}.
#' 
#' @param N An integer. Size of sample.
#' @param a A number. Parameter governing effect of treatment (Z) on mediator (M).
#' @param b A number. Effect of mediator (M) on outcome (Y) when Z = 0.
#' @param c A number. Interaction between mediator (M) and (Z) for outcome (Y).
#' @param d A number. Direct effect of treatment (Z) on outcome (Y), when M = 0.
#' @param rho A number in [-1,1]. Correlation between mediator (M) and outcome (Y) error terms. Non zero correlation implies a violation of sequential ignorability.
#' @param design_name A character vector. Name of design. This is the label of the design object returned by \code{get_design_code()}. Must be provided without spacing.
#' @param fixed A character vector. Names of arguments to be fixed in design.
#' @return A mediation analysis design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment
#' @concept mediation
#' @importFrom DeclareDesign declare_assignment declare_estimands declare_estimator declare_population declare_potential_outcomes declare_reveal declare_step diagnose_design draw_estimands
#' @importFrom fabricatr fabricate fabricate
#' @importFrom randomizr conduct_ra 
#' @importFrom estimatr lm_robust
#' @importFrom rlang eval_bare expr
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
mediation_analysis_designer <- function(N = 200, a = 1, b = .4, c = 0, d = .5, 
                                        rho = 0, design_name = "mediation_analysis_design",
                                        fixed = NULL)
{
  
  if(abs(rho) > 1) stop("rho must be in [-1, 1]")
  if(grepl(" ", design_name, fixed = TRUE)) "`design_name` may not contain any spaces."
  argument_names <- names(match.call.defaults(envir = parent.frame()))[-1]
  fixed_wrong <- fixed[!fixed %in% argument_names]
  if(length(fixed_wrong)!=0) stop(paste0("The following arguments in `fixed` do not match a designer argument:", fixed_wrong)) 
  
  fixed_txt <- fixed_expr(c("N", "a", "b", "c", "d", "rho"))
  for(i in 1:length(fixed_txt)) eval(parse(text = fixed_txt[i]))
  
  population_expr <- expr(declare_population(
    N = !!N_, 
    e1 = rnorm(!!N_),
    e2 = rnorm(n = !!N_, mean = !!rho_ * e1, sd = sqrt(1 - rho^2))))
    
  POs_M_expr <- expr(declare_potential_outcomes(M ~ 1*(!!a_ * Z + e1 > 0)))
  
  POs_Y_expr <- expr(declare_potential_outcomes(Y ~ !!d_ * Z + !!b_ * M + !!c_ * M * Z + e2,
                                           conditions = list(M = 0:1, Z = 0:1)))
  POs_Y_nat_0_expr <- expr(declare_potential_outcomes(
    Y_nat0_Z_0 = !!b_ * M_Z_0 + e2,
    Y_nat0_Z_1 = !!d_ + !!b_ * M_Z_0 + !!c_ * M_Z_0 + e2))
  
  POs_Y_nat_1_expr <- expr(declare_potential_outcomes(
    Y_nat1_Z_0 = !!b_ * M_Z_1 + e2,
    Y_nat1_Z_1 = !!d_ + !!b_ * M_Z_1 + !!c_ * M_Z_1 + e2))
  
  {{{
    # M: Model
    population <- eval_bare(population_expr)

    POs_M <- eval_bare(POs_M_expr)
    POs_Y <- eval_bare(POs_Y_expr)
    POs_Y_nat_0 <- eval_bare(POs_Y_nat_0_expr)
    POs_Y_nat_1 <- eval_bare(POs_Y_nat_1_expr)
    
    # I: Inquiry
    estimands <- declare_estimands(
      FirstStage = mean(M_Z_1 - M_Z_0), 
      Indirect_0 = mean(Y_M_1_Z_0 - Y_M_0_Z_0),
      Indirect_1 = mean(Y_M_1_Z_1 - Y_M_0_Z_1),
      Controlled_Direct_0 = mean(Y_M_0_Z_1 - Y_M_0_Z_0),
      Controlled_Direct_1 = mean(Y_M_1_Z_1 - Y_M_1_Z_0),
      Natural_Direct_0 = mean(Y_nat0_Z_1 - Y_nat0_Z_0),
      Natural_Direct_1 = mean(Y_nat1_Z_1 - Y_nat1_Z_0)
    )
    
    # D: Data strategy 
    assignment <- declare_assignment()
    reveal_M <- declare_reveal(M, Z)
    reveal_Y <- declare_reveal(Y, assignment_variable = c("M","Z"))
    reveal_nat0 <- declare_reveal(Y_nat0)
    reveal_nat1 <- declare_reveal(Y_nat1)
    manipulation <- declare_step(Not_M = 1 - M, handler = fabricate)
    
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
      POs_M + POs_Y + POs_Y_nat_0 + POs_Y_nat_1 +
      estimands + assignment + 
      reveal_M + reveal_Y + reveal_nat0 + reveal_nat1 + manipulation +
      mediator_regression + stage2_1 + stage2_2 + stage2_3
    
  }}}
  
  design_code <- construct_design_code(mediation_analysis_designer, match.call.defaults(),
                                       exclude_args = union(c("design_name", "fixed"), fixed),
                                       arguments_as_values = TRUE)
  
  design_code <- sub_expr_text(design_code, population_expr, POs_M_expr, POs_Y_expr,
                               POs_Y_nat_0_expr, POs_Y_nat_1_expr)
  design_code <- gsub("mediation_analysis_design <-", paste0(design_name, " <-"), design_code, fixed = TRUE)
  
  attr(mediation_analysis_design, "code") <- design_code
  
  mediation_analysis_design
}

attr(mediation_analysis_designer,"definitions") <- data.frame(
  names = c("N",  "a",  "b",  "c",  "d",  "rho", "design_name", "fixed"),
  class = c("integer", rep("numeric", 5), rep("character", 2)),
  min = c(1, rep(-Inf, 4), -1, NA, NA),
  max = c(1, rep(Inf, 4), 1, NA, NA)
)

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
<p> A mediation analysis design with sample size <code>N</code> that examines
the effect of treatment (Z) on mediator (M) and the effect of mediator (M) on 
outcome (Y) (given Z=0) as well as direct effect of treatment (Z) on outcome
(Y) (given M=0).

<p> Analysis is implemented using an interacted regression model.

<p> Error terms on mediator (M) and outcome (Y) correlated by <code>rho</code>
"


