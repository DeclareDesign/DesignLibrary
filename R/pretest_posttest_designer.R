#' Create a pretest-posttest design
#'
#' Produces a design in which an outcome Y is observed pre- and post-treatment.
#' The design allows for individual post-treatment outcomes to be correlated with pre-treatment outcomes
#' and for at-random missingness in the observation of post-treatment outcomes. 
#' @details 
#' 
#' See \href{https://declaredesign.org/r/designlibrary/articles/pretest_posttest.html}{vignette online}.
#' 
#' @param N An integer. Size of sample.
#' @param ate A number. Average treatment effect.
#' @param sd_1 Nonnegative number. Standard deviation of period 1 shocks.
#' @param sd_2 Nonnegative number. Standard deviation of period 2 shocks.
#' @param rho A number in [-1,1]. Correlation in outcomes between pre- and post-test.
#' @param attrition_rate A number in [0,1]. Proportion of respondents in pre-test data that appear in post-test data.
#' @param args_to_fix A character vector. Names of arguments to be args_to_fix in design.
#' @return A pretest-posttest design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment
#' @concept difference-in-differences
#' @concept baseline
#' @importFrom DeclareDesign declare_assignment declare_inquiry declare_estimator declare_population declare_potential_outcomes declare_reveal declare_step
#' @importFrom fabricatr fabricate fabricate
#' @importFrom randomizr conduct_ra 
#' @importFrom estimatr lm_robust
#' @export
#' @examples
#' # Generate a pre-test post-test design using default arguments:
#' pretest_posttest_design <- pretest_posttest_designer()
#'
pretest_posttest_designer <- function(N = 100,
                                      ate = .25,
                                      sd_1 = 1,
                                      sd_2 = 1,
                                      rho = .5,
                                      attrition_rate = .1,
                                      args_to_fix = NULL)
{
  if(rho < -1 || rho > 1) stop("'rho' must be a value in [-1, 1]")
  if(any(sd_1 < 0, sd_2 < 0)) stop("'sd_1' and 'sd_2' must be nonnegative")
  if(attrition_rate < 0 || attrition_rate > 1) stop("'attrition_rate' must be in [0,1]")
  {{{
    # M: Model
    population <- declare_population(
      N    = N,
      u_t1 = rnorm(N)*sd_1,
      u_t2 = rnorm(N, rho * scale(u_t1), sqrt(1 - rho^2))*sd_2,
      Y_t1 = u_t1
    )
    
    potential_outcomes <- declare_potential_outcomes(Y_t2 ~ u_t2 + ate * Z)
    
    # I: Inquiry
    estimand <- declare_inquiry(ATE = mean(Y_t2_Z_1 - Y_t2_Z_0))
    
    # D: Data Strategy
    assignment <- declare_assignment(Z = complete_ra(N))
    
    report     <- declare_assignment(R = complete_ra(N, prob = 1 - attrition_rate))
    reveal_t2 <- declare_reveal(Y_t2) 
    
    manipulation <- declare_step(difference = (Y_t2 - Y_t1), handler = fabricate)  
    
    # A: Answer Strategy
    pretest_lhs <- declare_estimator(
      difference ~ Z,
      model = lm_robust,
      inquiry = estimand,
      subset = R == 1,
      label = "Change score"
    )
    
    pretest_rhs <- declare_estimator(
      Y_t2 ~ Z + Y_t1,
      model = lm_robust,
      inquiry = estimand,
      subset = R == 1,
      label = "Condition on pretest"
    )
    
    posttest_only <- declare_estimator(
      Y_t2 ~ Z,
      model = lm_robust,
      inquiry = estimand,
      label = "Posttest only"
    )
    # Design
    pretest_posttest_design <- population + potential_outcomes + estimand + 
      assignment + reveal_t2 + report + manipulation +
      pretest_lhs + pretest_rhs + posttest_only
  }}}
  
  attr(pretest_posttest_design, "code") <- 
    construct_design_code(pretest_posttest_designer, args_to_fix = args_to_fix, match.call.defaults())
  
  pretest_posttest_design
}
attr(pretest_posttest_designer, "definitions") <- data.frame(
  names = c("N",  "ate",  "sd_1",  "sd_2",  "rho",  "attrition_rate", "args_to_fix"),
  tips  = c("Size of sample",
            "Average treatment effect",
            "Standard deviation of period 1 shocks",
            "Standard deviation of period 2 shocks",
            "Correlation in outcomes between pre- and post-test",
            "Proportion of respondents lost when using pre-test data",
            "Names of arguments to be args_to_fix"),
  class = c("integer", rep("numeric", 5), "character"),
  vector = c(rep(FALSE, 6), TRUE),
  min = c(2, -Inf, 0, 0, -1, 0, NA),
  max = c(Inf, Inf, Inf, Inf, 1, 1, NA),
  inspector_min = c(100, 0, 0, 0, -1, 0, NA),
  inspector_step = c(50, rep(.2, 4), .2, NA),
  stringsAsFactors = FALSE
)
attr(pretest_posttest_designer, "shiny_arguments") <- list(
  N = c(100, 50, 1000),
  ate = c(.25,0,.5),
  rho = c(.25,0,.5),
  attrition_rate = c(0,.1,.2)
)

attr(pretest_posttest_designer, "description") <- "
<p> A pretest-posttest design with sample of size <code>N</code>, average treatment effect of size <code>ate</code>, 
    and correlation between pre- and post-test outcomes equal to <code>rho</code>. The proportion of pre-test respondents  
   missing at random from the post-test follow-up can be set using <code>attrition_rate</code>.
"

