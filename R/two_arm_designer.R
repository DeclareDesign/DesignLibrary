#' Create a one-level two-arm design
#'
#' Builds a design with one treatment and one control arm.
#' Treatment effects can be specified either by providing \code{control_mean} and \code{treatment_mean}
#' or by specifying a \code{control_mean} and \code{ate}.
#' 
#' @details 
#' Units are assigned to treatment using complete random assignment. Potential outcomes are normally distributed according to the mean and sd arguments.
#' 
#' @param N An integer. Sample size.
#' @param assignment_prob A number in [0,1]. Probability of assignment to treatment.
#' @param control_mean A number. Average outcome in control.
#' @param control_sd A positive number. Standard deviation in control.
#' @param ate A number. Average treatment effect.
#' @param treatment_mean A number. Average outcome in treatment. Overrides \code{ate} if both specified.
#' @param treatment_sd  A nonnegative number. Standard deviation in treatment. By default equals \code{control_sd}.
#' @param rho A number in [-1,1]. Correlation between treatment and control outcomes.
#' @param args_to_fix A character vector. Names of arguments to be args_to_fix in design.
#' @return A simple two-arm design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment
#' @importFrom DeclareDesign declare_assignment declare_inquiry declare_estimator declare_population declare_potential_outcomes declare_reveal
#' @importFrom fabricatr fabricate 
#' @importFrom randomizr conduct_ra 
#' @importFrom stats rnorm
#' @importFrom rlang list2 expr eval_bare
#' @aliases simple_two_arm_designer
#' @export two_arm_designer simple_two_arm_designer
#'
#' @examples
#' # Generate a simple two-arm design using default arguments
#' two_arm_design <- two_arm_designer()

two_arm_designer <- function(N = 100,
                             assignment_prob = .5,
                             control_mean = 0,
                             control_sd = 1,
                             ate = 1,
                             treatment_mean = control_mean + ate,
                             treatment_sd = control_sd,
                             rho = 1,
                             args_to_fix = NULL
){
  if(treatment_mean != ate + control_mean) warning("`treatment_mean` is not consistent with `ate`+`control_mean`. Value provided in `treatment_mean` will override `ate` value.")
  if(control_sd < 0 ) stop("control_sd must be non-negative")
  if(assignment_prob < 0 || assignment_prob > 1) stop("assignment_prob must be in [0,1]")
  if(abs(rho) > 1) stop("rho must be in [-1,1]")
  {{{
    # M: Model
    population <- declare_population(
      N = N,
      u_0 = rnorm(N),
      u_1 = rnorm(n = N, mean = rho * u_0, sd = sqrt(1 - rho^2)))
    
    potential_outcomes <- declare_potential_outcomes(
      Y ~ (1-Z) * (u_0*control_sd + control_mean) + 
        Z     * (u_1*treatment_sd + treatment_mean))
    
    # I: Inquiry
    estimand <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))
    
    # D: Data Strategy
    assignment <- declare_assignment(prob = assignment_prob)
    
    reveal_Y    <- declare_reveal()
    
    # A: Answer Strategy
    estimator <- declare_estimator(Y ~ Z, inquiry = estimand)
    
    # Design
    two_arm_design <- population + potential_outcomes + estimand + assignment + reveal_Y + estimator
  }}}
  
  attr(two_arm_design, "code") <-
    construct_design_code(designer = two_arm_designer,
                          args = match.call.defaults(),
                          args_to_fix = args_to_fix,
                          exclude_args = union(c("ate", "args_to_fix"), args_to_fix),
                          arguments_as_values = TRUE)
  two_arm_design
}

attr(two_arm_designer, "definitions") <- data.frame(
  names = c("N", "assignment_prob", "control_mean", "control_sd", 
            "ate", "treatment_mean", "treatment_sd", "rho", "args_to_fix"),
  tips  = c("Sample size",
            "Probability of assignment to treatment",
            "Average outcome in control",
            "Standard deviation in control",
            "Average treatment effect",
            "Average outcome in treatment",
            "Standard deviation in treatment",
            "Correlation between treatment and control outcomes",
            "Arguments to fix in design"),
  class = c("integer", rep("numeric", 7), "character"),
  vector = c(rep(FALSE, 8), TRUE),
  min   = c(4, 0, -Inf, 0, -Inf, -Inf, 0, -1, NA),
  max   = c(Inf, 1, Inf, Inf, Inf, Inf, Inf, 1, NA),
  inspector_min = c(100, rep(0, 6), -1, NA),
  inspector_step = c(50, rep(.2, 7), NA),
  stringsAsFactors = FALSE
)

attr(two_arm_designer, "shiny_arguments") <- list(N = c(10, 20, 50), ate = c(0, .5)) 

attr(two_arm_designer, "description") <- "
<p> A simple two arm design of sample size <code>N</code> and with average treatment effect equal to <code>ate</code>.
"

simple_two_arm_designer <- function(...){
  .Deprecated("two_arm_designer")
  dots <- list2(...)
  eval_bare(expr(two_arm_designer(!!!dots)))
}


