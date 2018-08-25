#' Create a regression discontinuity design
#'
#' Builds a design with sample from population of size \code{N}. The average treatment effect local to the cutpoint is equal to \code{tau}. It allows for specification of the order of the polynomial regression (\code{poly_reg_order}), cutoff value on the running variable (\code{cutoff}), and size of bandwidth around the cutoff (\code{bandwidth}). By providing a vector of numbers to \code{control_coefs} and \code{treatment_coefs}, users can also specify polynomial regression coefficients that generate the expected control and treatment potential outcomes given the running variable.
#' @details 
#' See \href{https://declaredesign.org/library/articles/regression_discontinuity.html}{vignette online}.
#' 
#' @param N An integer. Size of population to sample from.
#' @param tau A number. Difference in potential outcomes functions at the threshold.
#' @param outcome_sd A positive number. The standard deviation of the outcome.
#' @param cutoff A number in (0,1). Threshold on running variable beyond which units are treated.
#' @param bandwidth A number. The value of the bandwidth on both sides of the threshold from which to include units.
#' @param control_coefs A vector of numbers. Coefficients for polynomial regression function that generates control potential outcomes. Order of polynomial is equal to length.
#' @param treatment_coefs A vector of numbers. Coefficients for polynomial regression function that generates treatment potential outcomes. Order of polynomial is equal to length.
#' @param poly_reg_order Integer greater than or equal to 1. Order of the polynomial regression used to estimate the jump at the cutoff.
#' @return A regression discontinuity design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept observational
#' @concept regression discontinuity
#' @importFrom DeclareDesign declare_estimand declare_estimator declare_population declare_potential_outcomes declare_reveal declare_sampling
#' @importFrom fabricatr fabricate 
#' @importFrom randomizr conduct_ra draw_rs 
#' @importFrom estimatr tidy lm_robust
#' @importFrom rlang is_integerish
#' @importFrom stats poly runif
#' @export
#' @examples
#' # Generate a regression discontinuity design using default arguments:
#' regression_discontinuity_design <- regression_discontinuity_designer()

regression_discontinuity_designer <- function(
  N = 1000,
  tau = .15,
  outcome_sd = .1,
  cutoff = .5,
  bandwidth = .5,
  control_coefs = c(.5,.5),
  treatment_coefs = c(-5,1),
  poly_reg_order = 4
){
  if(cutoff <= 0 || cutoff >= 1) stop("cutoff must be in (0,1).")
  if(poly_reg_order < 1) stop("poly_reg_order must be at least 1.")
  if(!is_integerish(poly_reg_order)) stop("poly_reg_order must be an integer.")
  if(length(control_coefs) < 1) stop("control_coefs must be a numeric vector of length > 0.")
  if(length(treatment_coefs) < 1) stop("treatment_coefs must be a numeric vector of length > 0.")
  if(outcome_sd < 0) stop("outcome_sd must be positive.")
  {{{
    # M: Model
    control <- function(X) {
      as.vector(poly(X, length(control_coefs), raw = T) %*% control_coefs)}
    treatment <- function(X) {
      as.vector(poly(X, length(treatment_coefs), raw = T) %*% treatment_coefs) + tau}
    population <- declare_population(
      N = N,
      X = runif(N,0,1) - cutoff,
      noise = rnorm(N,0,outcome_sd),
      Z = 1 * (X > 0))
    potential_outcomes <- declare_potential_outcomes(
      Y_Z_0 = control(X) + noise,
      Y_Z_1 = treatment(X) + noise)
    reveal_Y <- declare_reveal(Y)
    
    # I: Inquiry
    estimand <- declare_estimand(LATE = treatment(0) - control(0))
    
    # D: Data Strategy
    sampling <- declare_sampling(handler = function(data){
      subset(data,(X > 0 - abs(bandwidth)) & X < 0 + abs(bandwidth))})
    
    # A: Answer Strategy 
    estimator <- declare_estimator(
      formula = Y ~ poly(X, poly_reg_order) * Z,
      model = lm_robust,
      term = "Z",
      estimand = estimand)
    
    # Design
    regression_discontinuity_design <- 
      population + potential_outcomes + estimand + reveal_Y + sampling + estimator
  }}}
  
  attr(regression_discontinuity_design, "code") <- 
    construct_design_code(regression_discontinuity_designer, match.call.defaults())
  
  regression_discontinuity_design
}

attr(regression_discontinuity_designer,"shiny_arguments") <-
  list(
    N = c(1000, 50,100, 10000),
    tau = c(.15, 0, .5, 1),
    cutoff = c(.5,  .25),
    bandwidth = c(.5,  .25),
    poly_reg_order = c(4, 3, 2, 1)
  )

attr(regression_discontinuity_designer,"tips") <-
  c(N = "Size of population to sample from",
    tau = "Difference in potential outcomes functions at the threshold",
    cutoff = "Threshold on running variable beyond which units are treated",
    bandwidth = "Bandwidth around threshold from which to include units",
    poly_reg_order = "Order of the polynomial regression used to estimate the jump at the cutoff"
  )

attr(regression_discontinuity_designer,"description") <- "
<p> A regression discontinuity design with sample from population of size <code>N</code>. 
    The average treatment effect local to the cutpoint is equal to <code>tau</code>. 
<p> Polynomial regression of order <code>poly_order</code> is used to estimate tau, within a bandwidth of size
    <code>bandwidth</code> around the <code>cutoff</code> value on the running variable.
"


  