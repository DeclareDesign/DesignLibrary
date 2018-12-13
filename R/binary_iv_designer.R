#' Create a binary instrumental variables design
#'
#' Builds a design with one instrument, one binary explanatory variable, and one outcome.
#' 
#' A researcher is interested in the effect of binary X on outcome Y.  The relationship is confounded  because units that are more likely to be assigned to X=1 have higher Y outcomes.
#' A potential instrument Z is examined, which plausibly causes X. The instrument can be used to assess the effect of X on Y for units whose value of X depends on Z if Z does not negatively affect X for some cases, affects X positively for some, and affects Y only through X. 
#' 
#' 
#' @param N An integer. Sample size.
#' @param type_probs A vector of four numbers in [0,1]. Probability of each complier type (always-taker, never-taker, complier, defier).
#' @param assignment_probs A vector of four numbers in [0,1]. Probability of assignment to encouragement (Z) for each complier type (always-taker, never-taker, complier, defier). Under random assignment these are normally identical since complier status is not known to researchers in advance.
#' @param a_Y A real number. Constant in Y equation. Assumed constant across types. Overridden by \code{a} if specified.
#' @param a A vector of four numbers. Constant in Y equation for each complier type (always-taker, never-taker, complier, defier).
#' @param b_Y A real number. Effect of X on Y equation. Assumed constant across types. Overridden by \code{b} if specified. 
#' @param b A vector of four numbers. Slope on X in Y equation for each complier type (always-taker, never-taker, complier, defier).
#' @param d_Y A real number. Effect of Z on Y. Assumed constant across types. Overridden by \code{d} if specified.
#' @param d A vector of four numbers. Slope on Z in Y equation for each complier type (non zero implies violation of exclusion restriction).
#' @param outcome_sd A non negative number. Standard deviation on Y.
#' @param outcome_name A character. Name of outcome variable (defaults to "Y"). Must be provided without spacing inside the function \code{c()} as in \code{outcome_name = c("Conflict")}.
#' @param treatment_name A character. Name of outcome variable (defaults to "X"). Must be provided without spacing inside the function \code{c()} as in \code{outcome_name = c("GDP")}.
#' @param instrument_name A character. Name of outcome variable (defaults to "Z"). Must be provided without spacing inside the function \code{c()} as in \code{outcome_name = c("Rain")}.
#' @param design_name A character vector. Name of design. This is the label of the design object returned by \code{get_design_code()}. Must be provided without spacing inside the function \code{c()} as in \code{design_name = c("my_design")}.
#' @param fixed A character vector. Names of arguments to be fixed in design. \code{outcome_name}, \code{treatment_name} and \code{instrument_name} are always fixed.
#' @return A simple instrumental variables design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment
#' @importFrom DeclareDesign declare_estimand declare_estimator declare_population declare_potential_outcomes declare_reveal diagnose_design
#' @importFrom fabricatr fabricate 
#' @importFrom randomizr conduct_ra 
#' @importFrom generics tidy
#' @importFrom estimatr iv_robust lm_robust
#' @importFrom stats runif
#' @importFrom rlang list2 eval_bare expr exprs is_integerish parse_expr quo_text quos sym UQS
#' @aliases simple_iv_designer
#' @export binary_iv_designer simple_iv_designer
#'
#' @examples
#' # Generate a simple iv design: iv identifies late not ate 
#' binary_iv_design_1 <- binary_iv_designer(N = 1000, b = c(.1, .2, .3, .4))
#' \dontrun{
#' diagnose_design(binary_iv_design_1)
#' }
#' 
#' # Generates a simple iv design with violation of monotonicity
#' binary_iv_design_2 <- binary_iv_designer(type_probs = c(.1,.1,.6, .2), b_Y = .5)
#' \dontrun{
#' diagnose_design(binary_iv_design_2)
#' }
#' 
#' # Generates a simple iv design with violation of exclusion restriction
#' binary_iv_design_3 <- binary_iv_designer(d_Y = .5, b_Y = .5)
#' \dontrun{
#' diagnose_design(binary_iv_design_3)
#' }
#' 
#' # Generates a simple iv design with violation of randomization
#' binary_iv_design_4 <- binary_iv_designer(N = 1000, assignment_probs = c(.2, .3, .7, .5), b_Y = .5)
#' \dontrun{
#' diagnose_design(binary_iv_design_4)
#' }
#' 
#' # Generates a simple iv design with violation of first stage
#' binary_iv_design_5 <- binary_iv_designer(type_probs = c(.5,.5, 0, 0), b_Y = .5)
#' \dontrun{
#' diagnose_design(binary_iv_design_5)
#' }
#' 
binary_iv_designer <- function(N = 100, 
                               type_probs = c(1/3, 1/3, 1/3, 0), 
                               assignment_probs = c(.5, .5, .5, .5), 
                               a_Y = 1,
                               b_Y = 0,
                               d_Y = 0,
                               outcome_sd = 1,
                               a = c(1,0,0,0) * a_Y, 
                               b = rep(b_Y, 4), 
                               d = rep(d_Y, 4),
                               outcome_name = "Y",
                               treatment_name = "X",
                               instrument_name = "Z",
                               design_name = "binary_iv_design",
                               fixed = NULL
){
  if(min(assignment_probs) < 0 ) stop("assignment_probs must be non-negative.")
  if(max(assignment_probs) > 1 ) stop("assignment_probs must be < 1.")
  if(outcome_sd < 0) stop("outcome_sd must be positive.")
  if(length(a) != 4) stop("vector a must be length 4.")
  if(length(b) != 4) stop("vector b must be length 4.")
  if(length(d) != 4) stop("vector d must be length 4.")
  if(grepl(" ", design_name, fixed = TRUE)) "`design_name` may not contain any spaces."
  
  if(!"N" %in% fixed) N_ <- expr(N)
  if(!"type_probs" %in% fixed) type_probs_ <- expr(type_probs)
  if(!"assignment_probs" %in% fixed) assignment_probs_ <- expr(assignment_probs)
  if(!"outcome_sd" %in% fixed) outcome_sd_ <- expr(outcome_sd)
  if(!"a" %in% fixed) a_ <- expr(a)
  if(!"b" %in% fixed) b_ <- expr(b)
  if(!"d" %in% fixed) d_ <- expr(d)
  
  N_ <- N; assignment_probs_ <- assignment_probs; a_Y_ <- a_Y 
  b_Y_ <- b_Y; d_Y_ <- d_Y; outcome_sd_ <- outcome_sd
  a_ <- a; b_ <- b; d_ <- d
  
  type <- sym("type")
  
  population_args <- exprs(N = !!N_,
                           type = sample(1:4, N, replace = TRUE, prob = !!type_probs_),
                           type_label = c("Always", "Never", "Complier", "Defier")[type],
                           u_Z = runif(N),
                           u_Y = rnorm(N) * !!outcome_sd_,
                           !!instrument_name := (u_Z < (!!(assignment_probs_))[type]),
                           !!treatment_name := (type == 1) + (type == 3) * !!sym(instrument_name) + (type == 4) * (1 - !!sym(instrument_name)))
  
  population_expr <- expr(declare_population(
    !!!population_args
  ))
  
  potentials_expr <- expr(declare_potential_outcomes(
    !!sym(outcome_name) ~ a[type] + b[type] * !!sym(treatment_name) + d[type] * !!sym(instrument_name) + u_Y,
    assignment_variables = !!treatment_name))
  
  reveal_expr <- expr(declare_reveal(outcome_variables = !!outcome_name,
                                     assignment_variables = !!treatment_name))
  
  po_names <- sapply(paste(outcome_name, treatment_name, 0:1, sep = "_"), sym)
  
  estimand_expr <- expr(declare_estimand(
    first_stage = mean((type == 3) - (type == 4)),
    ate = mean(!!po_names[[2]] - !!po_names[[1]]),
    late = mean((!!po_names[[2]])[type == 3] - (!!po_names[[1]])[type == 3])
  ))
  
  estimator1_expr <- expr(declare_estimator(!!sym(treatment_name) ~ !!sym(instrument_name), 
                                            estimand = "first_stage", 
                                            label = "d-i-m"))
  estimator2_expr <- expr(declare_estimator(!!sym(outcome_name) ~ !!sym(treatment_name), 
                                            estimand = c("ate", "late"), 
                                            model = lm_robust, 
                                            label = "lm_robust"))
  estimator3_expr <- expr(declare_estimator(!!sym(outcome_name) ~ !!sym(treatment_name) | Z, 
                                            estimand = c("ate", "late"), 
                                            model = iv_robust, 
                                            label = "iv_robust"))
  
  {{{
    
    # Model
    population <- eval_bare(population_expr)
    
    potential_outcomes <- eval_bare(potentials_expr)
    
    reveal <- eval_bare(reveal_expr)
    
    # I: Inquiries
    estimand <- eval_bare(estimand_expr)
    
    # Answers
    estimator_1 <- eval_bare(estimator1_expr)
    estimator_2 <- eval_bare(estimator2_expr)
    estimator_3 <- eval_bare(estimator3_expr)
    
    binary_iv_design <- population + potential_outcomes + reveal + 
      estimand + estimator_1 + estimator_2 + estimator_3
    
  }}}
  
  design_code <- construct_design_code(binary_iv_designer,
                                       match.call.defaults(),
                                       arguments_as_values = TRUE,
                                       exclude_args = c("a_Y", "b_Y", "d_Y",
                                                        "outcome_name", "treatment_name",
                                                        "instrument_name", "design_name", fixed, "fixed"))
  
  design_code <- sub_expr_text(design_code, population_expr, potentials_expr,
                               reveal_expr, estimand_expr, estimator1_expr,
                               estimator2_expr, estimator3_expr)
  
  design_code <- gsub("binary_iv_design <-", paste0(design_name, " <-"), design_code, fixed = TRUE)
  
  attr(binary_iv_design, "code") <- design_code
  
  binary_iv_design
  
}

attr(binary_iv_designer, "definitions") <- data.frame(
  names = c("N", "type_probs", "assignment_probs", "a_Y", "b_Y", "d_Y",
            "outcome_sd", "a", "b", "d", "outcome_name", "treatment_name",
            "instrument_name", "design_name", "fixed"),
  class = c("integer", rep("numeric", 9), rep("character", 5)),
  min   = c(4, 0, 0, rep(-Inf, 3), 0, rep(-Inf, 3), rep(NA, 5)),
  max   = c(Inf, 1, 1, rep(Inf, 7), rep(NA, 5))
)


attr(binary_iv_designer, "shiny_arguments") <- list(design_name = "binary_iv_design",
                                                    N = c(10, 20, 50), b_Y = c(0,1), d_Y = c(0,1)) 

attr(binary_iv_designer, "tips") <-
  list(
    design_name = "Name of design",
    N = "Sample size",
    b_Y = "Effect of X on Y",
    d_Y = "Effect of Z on Y"
  )

attr(binary_iv_designer, "description") <- "
<p> A simple IV design of sample size <code>N</code>.
"

simple_iv_designer <- function(...){
  .Deprecated("binary_iv_designer")
  dots <- list2(...)
  eval_bare(expr(binary_iv_designer(!!!dots)))
}




