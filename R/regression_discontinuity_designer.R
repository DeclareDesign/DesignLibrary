#' Create a regression discontinuity design
#'
#' Builds a design with sample from population of size \code{N}. The average treatment effect local to the cutpoint is equal to \code{tau}. It allows for specification of the order of the polynomial regression (\code{poly_order}), cutoff value on the running variable (\code{cutoff}), and size of bandwidth around the cutoff (\code{bandwidth}).
#'
#' @param N An integer. Size of population to sample from.
#' @param tau A number. Difference in potential outcomes functions at the threshold.
#' @param cutoff A number in (0,1). Threshold on running variable beyond which units are treated.
#' @param bandwidth A number. Bandwidth around threshold from which to include units.
#' @param poly_order An integer. Order of the polynomial regression used to estimate the jump at the cutoff.
#' @return A regression discontinuity design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept observational
#' @concept regression discontinuity
#' @import DeclareDesign stats utils fabricatr estimatr randomizr
#' @export
#' @examples
#' # Generate a regression discontinuity design using default arguments:
#' regression_discontinuity_design <- regression_discontinuity_designer()

regression_discontinuity_designer <- function(
  N = 1000,
  tau = .15,
  cutoff = .5,
  bandwidth = .5,
  poly_order = 4
){
  X <- noise <- Y <- NULL
  if(! (cutoff < 1 & cutoff > 0)) stop("cutoff must be in (0,1)")
  if(poly_order < 1) stop("poly_order must be greater than 0.")
  {{{
    # M: Model
    control <- function(X) {
      as.vector(poly(X, 4, raw = T) %*% c(.7, -.8, .5, 1))}
    treatment <- function(X) {
      as.vector(poly(X, 4, raw = T) %*% c(0, -1.5, .5, .8)) + tau}
    population <- declare_population(
      N = N,
      X = runif(N,0,1) - cutoff,
      noise = rnorm(N,0,.1),
      Z = 1 * (X > 0))
    potentials <- declare_potential_outcomes(
      Y_Z_0 = control(X) + noise,
      Y_Z_1 = treatment(X) + noise)
    reveal_Y <- declare_reveal(Y)
    
    # I: Inquiry
    estimand <- declare_estimand(LATE = treatment(0) - control(0))
    
    # D: Data Strategy
    sampling <- declare_sampling(handler = function(data){
      subset(data,(X > 0 - bandwidth) & X < 0 + bandwidth)})
    
    # A: Answer Strategy 
    estimator <- declare_estimator(
      formula = Y ~ poly(X, poly_order) * Z,
      model = lm_robust,
      term = "Z",
      estimand = estimand)
    
    # Design
    regression_discontinuity_design <- 
      population + potentials + estimand + reveal_Y + sampling + estimator
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
    poly_order = c(4, 3, 2, 1)
  )

attr(regression_discontinuity_designer,"tips") <-
  c(N = "Size of population to sample from",
    tau = "Difference in potential outcomes functions at the threshold",
    cutoff = "Threshold on running variable beyond which units are treated",
    bandwidth = "Bandwidth around threshold from which to include units",
    poly_order = "Order of the polynomial regression used to estimate the jump at the cutoff"
  )

attr(regression_discontinuity_designer,"description") <- "
<p> A regression discontinuity design with sample from population of size <code>N</code>. 
    The average treatment effect local to the cutpoint is equal to <code>tau</code>. 
<p> Polynomial regression of order <code>poly_order</code> is used to estimate tau, within a bandwidth of size
    <code>bandwidth</code> around the cutoff situated at <code>cutoff</code> on the running variable.
"


  