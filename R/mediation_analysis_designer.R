#' Create a design for mediation analysis
#' 
#' A mediation analysis design that examines the effect of (Z) on mediator (M), the natural and controlled direct effect of treatment (Z) on outcome (Y) as well as the natural and controlled indirect effect of treatment (Z) on outcome (Y) through mediator (M).
#' Analysis is implemented using a set of two linear structural models: a first stage model and a interacted model. Note estimates are not guaranteed to be unbiased despite randomization of Z because of possible violations of sequential ignorability. 
#' 
#' 
#' @details 
#' See \href{https://declaredesign.org/library/articles/mediation_analysis.html}{vignette online}.
#' 
#' @param N An integer. Size of sample.
#' @param Z_on_M A number. Parameter governing effect of treatment (Z) on mediator (M).
#' @param M_on_Y_Z0 A number. Effect of mediator (M) on outcome (Y) when Z = 0.
#' @param M_on_Y_Z1 A number. Interaction between mediator (M) and (Z) for outcome (Y).
#' @param Z_on_Y_M0 A number. Effect of treatment (Z) on outcome (Y), when M = 0.
#' @param rho A number in [-1,1]. Correlation between mediator (M) and outcome (Y) error terms. Non zero correlation implies a violation of sequential ignorability.
#' @param mediation_package A logical value. If 'TRUE' direct and indirect effects are estimated using \code{mediate} function from \code{mediation} package. Default is 'FALSE'.  
#' @return A mediation analysis design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment
#' @concept mediation
#' @importFrom DeclareDesign declare_assignment declare_estimands declare_estimator declare_population declare_potential_outcomes declare_reveal declare_step diagnose_design draw_estimands
#' @importFrom fabricatr fabricate fabricate
#' @importFrom randomizr conduct_ra
#' @importFrom stats lm
#' @importFrom estimatr tidy lm_robust
#' @importFrom broom tidy
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
#' mediation_2 <- mediation_analysis_designer(Z_on_M =1, rho = .5, M_on_Y_Z1 = 1, Z_on_Y_M0  =.75)
#' draw_estimands(mediation_2)
#' \dontrun{
#' diagnose_design(mediation_2, sims = 1000)
#' }
#'
#'
mediation_analysis_designer <- function(N = 200,
                                        Z_on_M = 1, 
                                        M_on_Y_Z0 = .4, 
                                        M_on_Y_Z1 = 0, 
                                        Z_on_Y_M0 = .5, 
                                        rho = 0, 
                                        mediation_package = FALSE)
  
{
  
  if(abs(rho) > 1) stop("rho must be in [-1, 1]")
  
  mediation_analysis_expr  <- mediate_estimator_expr <-  NULL
  
  # I: Inquiry
  
  estimands_expr <- rlang::expr(
    estimands <- declare_estimands(
      FirstStage = mean(M_Z_1 - M_Z_0),
      natural_indirect_0 = mean(Y_nat1_Z_0 - Y_nat0_Z_0),
      natural_direct_0  = mean(Y_nat0_Z_1 - Y_nat0_Z_0),
      natural_direct_1  = mean(Y_nat1_Z_1 - Y_nat1_Z_0),
      controlled_indirect_0 = mean(Y_M_1_Z_0 - Y_M_0_Z_0),
      controlled_direct_0  = mean(Y_M_0_Z_1 - Y_M_0_Z_0),
      controlled_direct_1  = mean(Y_M_1_Z_1 - Y_M_1_Z_0)
    )
  ) 
  
  # Design
  
  mediation_design_expr <-  rlang::expr(
    mediation_analysis_design <- population + 
      POs_M + POs_Y + POs_Y_nat_0 + POs_Y_nat_1 +
      estimands + assignment + 
      reveal_M + reveal_Y + reveal_nat0 + reveal_nat1 + manipulation +
      mediator_regression + stage2_1 + stage2_2 + stage2_3 
  )
  
  if(mediation_package){
    stopifnot(requireNamespace("mediation"))
    
    # I: Inquiry
    estimands_expr <- rlang::expr(
      estimands <- declare_estimands(
        FirstStage = mean(M_Z_1 - M_Z_0),
        natural_indirect_0 = mean(Y_nat1_Z_0 - Y_nat0_Z_0),
        natural_indirect_1 = mean(Y_nat1_Z_1  - Y_nat0_Z_1),
        natural_direct_0 = mean(Y_nat0_Z_1 - Y_nat0_Z_0),
        natural_direct_1 = mean(Y_nat1_Z_1 - Y_nat1_Z_0),
        controlled_indirect_0 = mean(Y_M_1_Z_0 - Y_M_0_Z_0) ,
        controlled_indirect_1 = mean(Y_M_1_Z_1 - Y_M_0_Z_1),
        controlled_direct_0  = mean(Y_M_0_Z_1 - Y_M_0_Z_0),
        controlled_direct_1  = mean(Y_M_1_Z_1 - Y_M_1_Z_0)
      )
    )
    
    # A: Answer Strategy
    
    
    
    
    mediate_estimator_expr <-  rlang::expr(
      mediate_estimator <- declare_estimator(handler = function(data){
        
        # QBA: Quasi-Bayesian Approximation
        e1 <- lm(M ~ Z, data = data)
        e2 <- lm(Y ~ M + Z + M:Z, data = data)
        m  <- mediation::mediate(e1, e2, sims = 100, treat = "Z", mediator = "M")
        
        estimates <-   tidy(m, conf.int = TRUE)
        estimates <-  rbind(estimates, estimates)
        estimates$estimator_label <-  rep(c("qba - indirect_0", "qba - indirect_1", "qba - direct_0", "qba - direct_1") , 2)
        estimates$estimand_label <- c("natural_indirect_0", "natural_indirect_1", "natural_direct_0", "natural_direct_1",
                                      c("controlled_indirect_0", "controlled_indirect_1", "controlled_direct_0", "controlled_direct_1") ) 
        estimates$outcome <- rep("Y", 4)
        estimates$term <- rep(c("indirect_0", "indirect_1", "direct_0", "direct_1"), 2)
        as.data.frame(estimates)
      },
      label = "mediate")
    )
    
    # Design
    mediation_design_expr <-  rlang::expr(
      population + 
        POs_M + POs_Y + POs_Y_nat_0 + POs_Y_nat_1 +
        estimands + assignment + 
        reveal_M + reveal_Y + reveal_nat0 + reveal_nat1 + manipulation +
        mediator_regression + stage2_1 + stage2_2 + stage2_3  +  mediate_estimator
    )
  } 
  
  
  
  {{{
    
    # M: Model
    
    population <- declare_population(
      N = N, 
      e1 = rnorm(N),
      e2 = rnorm(n = N, mean = rho * e1, sd = sqrt(1 - rho^2))
    )
    
    POs_M <- declare_potential_outcomes(M ~ 1 * (Z_on_M * Z  +  e1 > 0))
    POs_Y <- declare_potential_outcomes(Y ~ Z_on_Y_M0 * Z  +  M_on_Y_Z0 * M  +  M_on_Y_Z1 * M * Z  +  e2,
                                        conditions = list(M = 0:1, Z = 0:1))
    
    POs_Y_nat_0 <- declare_potential_outcomes(
      Y_nat0_Z_0 = M_on_Y_Z0  *  M_Z_0  +  e2,
      Y_nat0_Z_1 = Z_on_Y_M0  +  M_on_Y_Z0 * M_Z_0  +  M_on_Y_Z1 * M_Z_0  +  e2)
    POs_Y_nat_1 <- declare_potential_outcomes(
      Y_nat1_Z_0 = M_on_Y_Z0 * M_Z_1  +  e2,
      Y_nat1_Z_1 = Z_on_Y_M0  +  M_on_Y_Z0 * M_Z_1  +  M_on_Y_Z1 * M_Z_1  +  e2)
    
    # I: Inquiry
    
    rlang::eval_bare(estimands_expr)
    
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
      label = "Stage 1",
      estimand = "FirstStage"
    )
    
    
    stage2_1 <- declare_estimator(
      Y ~ Z  *  M,
      model = lm_robust,
      term = c("M"),
      label = "Stage 2",
      estimand = c( "natural_indirect_0",  "controlled_indirect_0")
    )
    
    stage2_2 <- declare_estimator(
      Y ~ Z  *  M,
      model = lm_robust,
      term = c("Z"),
      label = "Direct_0",
      estimand = c("natural_direct_0", "controlled_direct_0")
    )
    
    stage2_3 <- declare_estimator(
      Y ~ Z  *  Not_M,
      model = lm_robust,
      term = c("Z"),
      label = "Direct_1",
      estimand = c("natural_direct_1", "controlled_direct_1")
    )
    
    
    rlang::eval_bare(mediate_estimator_expr)
    
    # Design
    
    mediation_analysis_design <- rlang::eval_bare(mediation_design_expr)
    
    
  }}}
  
  design_code <-
    construct_design_code(
      mediation_analysis_designer,
      match.call.defaults(),
      arguments_as_values = TRUE,
      exclude_args = "mediation_package"
    )
  
  if(mediation_package) 
    substitutes <- list(rlang::quo_text(mediation_analysis_expr), rlang::quo_text(mediate_estimator_expr))
  else 
    substitutes <- list("", "")
  
  
  design_code <-
    gsub("rlang::eval_bare\\(mediation_analysis_expr\\)", substitutes[[1]], design_code)
  design_code <-
    gsub("rlang::eval_bare\\(mediate_estimator_expr\\)",  substitutes[[2]], design_code)
  
  design_code <-
    gsub("rlang::eval_bare\\(mediation_design_expr\\)",  rlang::quo_text(mediation_design_expr), design_code)
  design_code <-
    gsub("rlang::eval_bare\\(estimands_expr\\)",  rlang::quo_text(estimands_expr), design_code)
  
  attr(mediation_analysis_design, "code") <- design_code
  return(mediation_analysis_design)
  
}


attr(mediation_analysis_designer,"shiny_arguments") <- list(
  N = c(100, 50, 1000),
  Z_on_M = seq(from = .5, to = -.5, by = -.5),
  M_on_Y_Z0 = seq(from = .5, to = -.5, by = -.5),
  M_on_Y_Z1 = seq(from = .5, to = -.5, by = -.5),
  Z_on_Y_M0 = seq(from = .5, to = -.5, by = -.5), 
  rho = c(.2, seq(from = -1, to = 1, by = .5)),
  mediation_package =  c(FALSE, TRUE)
)


attr(mediation_analysis_designer,"definitions") <- data.frame(
  names = c("N",  "Z_on_M",  "M_on_Y_Z0 ",  "M_on_Y_Z1",  "Z_on_Y_M0",  "rho", "mediation_package"),
  tips  = c("Size of sample",
            "Effect of treatment (Z) on mediator (M)",
            "Effect of mediator (M) on outcome (Y) when Z = 0.",
            "Interaction between mediator (M) and (Z) for outcome (Y)",
            "Effect of treatment (Z) on outcome (Y), when M = 0",
            "Correlation of mediator (M) and outcome (Y) error terms",
            "If 'TRUE' direct and indirect effects are estimated using mediation::mediate()"),
  class = c("integer", rep("numeric", 5), "logical"),
  min = c(1, rep(-Inf, 4), -1, 0),
  max = c(1, rep(Inf, 4), 1, 1),
  inspector_min = c(100, rep(0, 4), -1, 0),
  inspector_step = c(50, 0.1, .2, 0.3, 0.4, 0.2, 0),
  stringsAsFactors = FALSE
)

attr(mediation_analysis_designer,"description") <- "
<p> A mediation analysis design with sample size <code>N</code> that examines
the effect of treatment (Z) on mediator (M) and the effect of mediator (M) on 
outcome (Y) (given Z=0) as well as direct effect of treatment (Z) on outcome
(Y) (given M=0).

<p> Analysis is implemented using an interacted regression model.

<p> Error terms on mediator (M) and outcome (Y) correlated by <code>rho</code>
"
