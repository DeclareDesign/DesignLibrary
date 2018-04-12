#' Create a simple two arm design
#' @param N Number of units
#' @param code If TRUE designer returns the code of a design 
#' @return a function that returns a design
#' @export
#'
#' @examples
#' simple_two_arm_design <- simple_two_arm_designer()
#' simple_two_arm_designer(code = TRUE)

#' @export
regression_discontinuity_designer <- function(
  N = 1000,
  tau = .15,
  cutoff = .5,
  bandwidth = .5,
  poly_order = 4,
  code = FALSE
){
  design_code <- function(){  
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
      potential_outcomes <- declare_potential_outcomes(
        Y_Z_0 = control(X) + noise,
        Y_Z_1 = treatment(X) + noise)
      
      # I: Inquiry
      estimand <- declare_estimand(LATE = treatment(0) - control(0))
      
      # D: Data Strategy
      sampling <- declare_sampling(handler = function(data){
        subset(data,(X > 0 - bandwidth) & X < 0 + bandwidth)})
      
      # A: Answer Strategy 
      estimator <- declare_estimator(
        formula = Y ~ poly(X, poly_order) * Z,
        model = lm_robust,
        coefficients = "Z",
        estimand = estimand)
      
      # Design
      regression_discontinuity_design <- 
        pop / pos / estimand / declare_reveal(Y) / sampling / estimator
    }}}
    regression_discontinuity_design
  }
  if(code)  out <- get_design_code(design_code)
  if(!code) out <- design_code()
  return(out)
}

attr(regression_discontinuity_designer,"shiny_arguments") <-
  list(
    N = c(1000, 50,100,250, 500, 2500, 5000, 10000),
    tau = c(.15, 0, -1, -.5, -.15,  .5, 1),
    cutoff = c(.5, .01, .1, .25, .75, .9, .99),
    bandwidth = c(.5, .01, .1, .25, .75, .9, .99),
    poly_order = c(4, 3, 2, 1, 8)
  )

attr(regression_discontinuity_designer,"tips") <-
  c(N = "Size of population to sample from",
    tau = "Difference in potential outcomes functions at the threshold",
    cutoff = "Threshold on running variable beyond which units are treated",
    bandwidth = "Bandwidth around threshold from which to include units",
    poly_order = "Order of the polynomial regression used to estimate the jump at the cutoff"
  )
