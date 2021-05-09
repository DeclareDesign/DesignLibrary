#' Create a binary instrumental variables design
#'
#' Builds a design with one instrument, one binary explanatory variable, and one outcome.
#' 
#' A researcher is interested in the effect of binary X on outcome Y.  The relationship is confounded  because units that are more likely to be assigned to X=1 have higher Y outcomes.
#' A potential instrument Z is examined, which plausibly causes X. The instrument can be used to assess the effect of X on Y for units whose value of X depends on Z if Z does not negatively affect X for some cases, affects X positively for some, and affects Y only through X. 
#' 
#' @details 
#' See \href{https://declaredesign.org/r/designlibrary/articles/binary_iv.html}{vignette online} for more details on estimands.
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
#' @param outcome_sd A real number. The standard deviation of the outcome.
#' @param args_to_fix A character vector. Names of arguments to be args_to_fix in design.
#' @return A simple instrumental variables design with binary instrument, treatment, and outcome variables.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment
#' @importFrom DeclareDesign declare_inquiry declare_estimator declare_population declare_potential_outcomes declare_reveal diagnose_design
#' @importFrom fabricatr fabricate 
#' @importFrom randomizr conduct_ra 
#' @importFrom generics tidy
#' @importFrom estimatr iv_robust lm_robust
#' @importFrom stats runif
#' @importFrom rlang list2 expr eval_bare
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
                               args_to_fix = NULL
){
  if(min(assignment_probs) < 0 ) stop("assignment_probs must be non-negative.")
  if(max(assignment_probs) > 1 ) stop("assignment_probs must be < 1.")
  if(outcome_sd < 0) stop("outcome_sd must be positive.")
  if(length(a) != 4) stop("vector a must be length 4.")
  if(length(b) != 4) stop("vector b must be length 4.")
  if(length(d) != 4) stop("vector d must be length 4.")
  
  {{{
    
    # M: Model
    population <- declare_population(
      N = N,
      type = sample(1:4, N, replace = TRUE, prob = type_probs),
      type_label = c("Always", "Never", "Complier", "Defier")[type],
      u_Z = runif(N),
      u_Y = rnorm(N) * outcome_sd,
      Z = (u_Z < assignment_probs[type]),
      X = (type == 1) + (type == 3) * Z + (type == 4) * (1 - Z)
    )
    
    potential_outcomes <-
      declare_potential_outcomes(Y ~ a[type] + b[type] * X + d[type] * Z + u_Y,
                                 assignment_variables = "X")
    
    reveal <- declare_reveal(outcome_variables = Y,
                             assignment_variables = "X")
    
    # I: Inquiry
    estimand <- declare_inquiry(
      first_stage = mean((type == 3) - (type == 4)),
      ate = mean(Y_X_1 - Y_X_0),
      late = mean(Y_X_1[type == 3] - Y_X_0[type == 3]),
      late_het = (mean(type == 3)*mean(Y_X_1[type == 3] - Y_X_0[type == 3]) -
                         mean(type == 4)*mean(Y_X_1[type == 4] - Y_X_0[type == 4]))/(mean(type == 3) - mean(type == 4))
    )
    
    # A: Answer Strategy
    estimator_1 <- declare_estimator(X ~ Z, 
                                     model = difference_in_means,
                                     inquiry = "first_stage", 
                                     label = "d-i-m")
    
    estimator_2 <- declare_estimator(Y ~ X, 
                                     inquiry = c("ate", "late","late_het"), 
                                     model = lm_robust, 
                                     label = "lm_robust")
    
    estimator_3 <- declare_estimator(Y ~ X | Z, 
                                     inquiry = c("ate", "late", "late_het"), 
                                     model = iv_robust, 
                                     label = "iv_robust")
    
    
    binary_iv_design <- population + potential_outcomes + reveal + 
      estimand + estimator_1 + estimator_2 + estimator_3
    
  }}}
  
  attr(binary_iv_design, "code") <- 
    construct_design_code(designer = binary_iv_designer, 
                          args = match.call.defaults(),
                          args_to_fix = args_to_fix,
                          exclude_args = union(c("a_Y", "b_Y", "d_Y", "args_to_fix"), args_to_fix),
                          arguments_as_values = TRUE)
  
  binary_iv_design 
  
}

attr(binary_iv_designer, "shiny_arguments") <- list(N = c(80, 120, 160), b_Y = c(0,1), d_Y = c(0,1)) 

attr(binary_iv_designer, "definitions") <- data.frame(
  names         = c("N", "type_probs", "assignment_probs", "a_Y", "b_Y", "d_Y",
                    "outcome_sd", "a", "b", "d", "args_to_fix"),
  tips          = c("Sample size",
                    "Probability of each complier type (order: always-taker, never-taker, complier, defier)",
                    "Probability of assignment to encouragement", 
                    "Constant in Y equation for each complier type (order: always-taker, never-taker, complier, defier)",
                    "Effect of X on Y",
                    "Effect of Z on Y",
                    "Standard deviation of the outcome",
                    "Constant in Y equation", 
                    "Slope on X in Y equation for each complier type", 
                    "Slope on Z in Y equation for each complier type",
                    "Names of arguments to be fixed"),
  class         = c("integer", rep("numeric", 9), "character"),
  vector = c(FALSE, TRUE, TRUE, rep(FALSE, 4), TRUE, TRUE, TRUE, TRUE),
  min           = c(4, 0, 0, rep(-Inf, 3), 0, rep(-Inf, 3), NA),
  max           = c(Inf, 1, 1, rep(Inf, 7), NA),
  inspector_min = c(100, .1, .1, rep(0, 7), NA),
  inspector_step= c(100, .1, .1, rep(.1, 7), NA),
  stringsAsFactors = FALSE
)

attr(binary_iv_designer, "description") <- "
<p> A simple IV design of sample size <code>N</code>.
"

simple_iv_designer <- function(...){
  .Deprecated("binary_iv_designer")
  dots <- list2(...)
  eval_bare(expr(binary_iv_designer(!!!dots)))
}




