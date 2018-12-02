#' Create a simple two arm design with a possibly prognostic covariate
#'
#' Builds a design with one treatment and one control arm.
#' Treatment effects can be specified either by providing \code{control_mean} and \code{treatment_mean}
#' or by specifying a \code{control_mean} and \code{ate}.
#' Non random assignment is specified by a possible correlation, \code{rho_WZ},  between \code{W} and a latent variable that determines the probability of \code{Z}.  
#' Nonignorability is specified by a possible correlation, \code{rho_WY},  between \code{W} and outcome \code{Y}.  
#' 
#' 
#' @details 
#' Units are assigned to treatment using complete random assignment. Potential outcomes are normally distributed according to the mean and sd arguments.
#' 
#' See \href{https://declaredesign.org/library/articles/simple_two_arm.html}{vignette online}.
#' 
#' @param N An integer. Sample size.
#' @param prob A number in [0,1]. Probability of assignment to treatment.
#' @param control_mean A number. Average outcome in control.
#' @param sd A positive number. Standard deviation of shock on Y.
#' @param ate A number. Average treatment effect.
#' @param h A number. Controls heterogeneous treatment effects by W. Defaults to 0.  
#' @param treatment_mean A number. Average outcome in treatment. Overrides \code{ate} if both specified.
#' @param rho_WZ A number in [-1,1]. Correlation between W and Z.
#' @param rho_WY A number in [-1,1]. Correlation between W and Y.
#' @return A simple two-arm design with covariate W.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment
#' @importFrom DeclareDesign declare_assignment declare_estimand declare_estimator declare_population declare_potential_outcomes declare_reveal
#' @importFrom fabricatr fabricate 
#' @importFrom randomizr conduct_ra 
#' @importFrom estimatr lm_lin lm_robust difference_in_means 
#' @importFrom stats rnorm
#' @export
#'
#' @examples
#' #Generate a simple two-arm design using default arguments
#' two_arm_covariate_design <- two_arm_covariate_designer()
#' # Design with no confounding but a prognostic covariate 
#' prognostic <- two_arm_covariate_designer(N = 40, ate = .2, rho_WY = .9, h = .5)
#' \dontrun{
#' diagnose_design(prognostic)
#' }
#' # Design with confounding 
#' confounding <- two_arm_covariate_designer(N = 40, ate = 0, rho_WZ = .9, rho_WY = .9, h = .5)
#' \dontrun{
#' diagnose_design(confounding, sims = 2000)
#' }
#'
#' # Curse of power: A biased design may be more likely to mislead the larger it is 
#' curses <- expand_design(two_arm_covariate_designer, 
#'                         N = c(50, 500, 5000), ate = 0, rho_WZ = .2, rho_WY = .2)
#' \dontrun{
#' diagnoses <- diagnose_design(curses)
#' subset(diagnoses$diagnosands_df, estimator_label == "No controls")[,c("N", "power")]
#' }



two_arm_covariate_designer <- function(N = 100,
                                    prob = .5,
                                    control_mean = 0,
                                    sd = 1,
                                    ate = 1,
                                    h = 0,
                                    treatment_mean = control_mean + ate,
                                    rho_WY = 0,
                                    rho_WZ = 0
){
  if(sd < 0 ) stop("sd must be non-negative")
  if(prob < 0 || prob > 1) stop("prob must be in [0,1]")
  if(abs(rho_WY) > 1) stop("rho_WY must be in [-1,1]")
  if(abs(rho_WZ) > 1) stop("rho_WX must be in [-1,1]")
  {{{
    # M: Model
    population <- declare_population(
      N = N,
      u_W = rnorm(N),
      u_Y = rnorm(n = N, mean = rho_WY * u_W, sd = sqrt(1 - rho_WY^2)),
      u_Z = rnorm(n = N, mean = rho_WZ * u_W, sd = sqrt(1 - rho_WZ^2)),
      W   = u_W
      )
    
    potentials <- declare_potential_outcomes(
      Y ~ (1-Z) * (u_Y*sd + control_mean) + 
          Z     * (u_Y*sd + treatment_mean + h*u_W))
    
    # I: Inquiry
    estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
    
    # D: Data Strategy
    assignment  <- declare_step(Z = 1*(u_Z <  qnorm(prob)), handler = fabricate)
    reveal_Y    <- declare_reveal()
    
    # A: Answer Strategy
    estimator_1 <- declare_estimator(Y ~ Z,   estimand = estimand, 
                                     label = "No controls")
    estimator_2 <- declare_estimator(Y ~ Z+W, estimand = estimand, model = lm_robust, 
                                     label = "With controls")
    estimator_3 <- declare_estimator(Y ~ Z, covariates = ~ W, estimand = estimand, model = lm_lin,
                                     label = "Lin")
    
    # Design
    two_arm_covariate_design <- population + potentials + estimand + assignment + reveal_Y + 
      estimator_1 + estimator_2 +     estimator_3
  }}}
  
  attr(two_arm_covariate_design, "code") <- 
    construct_design_code(designer = two_arm_covariate_designer, 
                          args = match.call.defaults(), 
                          exclude_args = "ate",
                          arguments_as_values = TRUE)
  
  two_arm_covariate_design
}

attr(two_arm_covariate_designer, "shiny_arguments") <- list(N = c(10, 20, 50), ate = c(0, .5), 
                                                            rho_WZ = c(0, .5), rho_WY = c(0, .5)) 

attr(two_arm_covariate_designer, "tips") <-
  list(
    N = "Sample size",
    ate = "The average treatment effect",
    rho_WY = "Correlation between shock on Y and W",
    rho_WZ = "Correlation between shock on Y and latent variable for Z assignment"
  )

attr(two_arm_covariate_designer, "description") <- "
<p> A simple two arm designer with covariate <code>W</code>,  sample size <code>N</code>, independent assignment, and constant average treatment effect equal to <code>ate</code>.
"


