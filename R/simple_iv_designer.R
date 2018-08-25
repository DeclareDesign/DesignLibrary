#' Create a simple instrumental variables design
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
#' @return A simple instrumental variables design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment
#' @importFrom DeclareDesign declare_estimand declare_estimator declare_population declare_potential_outcomes declare_reveal diagnose_design
#' @importFrom fabricatr fabricate 
#' @importFrom randomizr conduct_ra 
#' @importFrom estimatr tidy iv_robust lm_robust
#' @importFrom stats runif
#' @export
#'
#' @examples
#' # Generate a simple iv design: iv identifies late not ate 
#' simple_iv_design_1 <- simple_iv_designer(N = 1000, b = c(.1, .2, .3, .4))
#' \dontrun{
#' diagnose_design(simple_iv_design_1)
#' }
#' 
#' # Generates a simple iv design with violation of monotonicity
#' simple_iv_design_2 <- simple_iv_designer(type_probs = c(.1,.1,.6, .2), b_Y = .5)
#' \dontrun{
#' diagnose_design(simple_iv_design_2)
#' }
#' 
#' # Generates a simple iv design with violation of exclusion restriction
#' simple_iv_design_3 <- simple_iv_designer(d_Y = .5, b_Y = .5)
#' \dontrun{
#' diagnose_design(simple_iv_design_3)
#' }
#' 
#' # Generates a simple iv design with violation of randomization
#' simple_iv_design_4 <- simple_iv_designer(N = 1000, assignment_probs = c(.2, .3, .7, .5), b_Y = .5)
#' \dontrun{
#' diagnose_design(simple_iv_design_4)
#' }
#' 
#' # Generates a simple iv design with violation of first stage
#' simple_iv_design_5 <- simple_iv_designer(type_probs = c(.5,.5, 0, 0), b_Y = .5)
#' \dontrun{
#' diagnose_design(simple_iv_design_5)
#' }
#' 
simple_iv_designer <- function(N = 100, 
                               type_probs = c(1/3, 1/3, 1/3, 0), 
                               assignment_probs = c(.5, .5, .5, .5), 
                               a_Y = 1,
                               b_Y = 0,
                               d_Y = 0,
                               outcome_sd = 1,
                               a = c(1,0,0,0) * a_Y, 
                               b = rep(b_Y, 4), 
                               d = rep(d_Y, 4) 
){
  if(min(assignment_probs) < 0 ) stop("assignment_probs must be non-negative.")
  if(max(assignment_probs) > 1 ) stop("assignment_probs must be < 1.")
  if(outcome_sd < 0) stop("outcome_sd must be positive.")
  if(length(a) != 4) stop("vector a must be length 4.")
  if(length(b) != 4) stop("vector b must be length 4.")
  if(length(d) != 4) stop("vector d must be length 4.")
  {{{
    
    # Model
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
    
    # I: Inquiries
    estimand <- declare_estimand(
      first_stage = mean((type == 3) - (type == 4)),
      ate = mean(Y_X_1 - Y_X_0),
      late = mean(Y_X_1[type == 3] - Y_X_0[type == 3])
    )
    
    # Answers
    estimator_1 <- declare_estimator(X ~ Z, 
                                     estimand = "first_stage", 
                                     label = "d-i-m")
    estimator_2 <- declare_estimator(Y ~ X, 
                                     estimand = c("ate", "late"), 
                                     model = lm_robust, 
                                     label = "lm_robust")
    estimator_3 <- declare_estimator(Y ~ X | Z, 
                                     estimand = c("ate", "late"), 
                                     model = iv_robust, 
                                     label = "iv_robust")
    
    
    simple_iv_design <- population + potential_outcomes + reveal + 
      estimand + estimator_1 + estimator_2 + estimator_3
    
  }}}
  
  attr(simple_iv_design, "code") <- 
    construct_design_code(designer = simple_iv_designer, 
                          args = match.call.defaults(), 
                          exclude_args = c("a_Y", "b_Y", "d_Y"),
                          arguments_as_values = TRUE)
  
  simple_iv_design 
  
}

attr(simple_iv_designer, "shiny_arguments") <- list(N = c(10, 20, 50), b_Y = c(0,1), d_Y = c(0,1)) 

attr(simple_iv_designer, "tips") <-
  list(
    N = "Sample size",
    b_Y = "Effect of X on Y",
    d_Y = "Effect of Z on Y"
  )

attr(simple_iv_designer, "description") <- "
<p> A simple IV design of sample size <code>N</code>.
"