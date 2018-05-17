#' Create a pretest-posttest design
#'
#' Description here
#'
#' Key limitations: Limitations here.
#'
#' Note: Note here.
#'
#' @param N An integer. Size of sample.
#' @param ate A number. Average treatment effect
#' @param rho A number in [0,1]. Correlation in outcomes between pre- and post-test
#' @param attrition_rate A number in [0,1]. Proportion of respondents lost when using pre-test data
#' @return A pretest-posttest design.
#' @author  DeclareDesign Team \url{https://declaredesign.org/}
#' @export
#' @examples
#' # To make a design using default arguments:
#' pretest_posttest_design <- pretest_posttest_designer()
#'
pretest_posttest_designer <- function(N = 100,
                                      ate = .25,
                                      rho = .5,
                                      attrition_rate = .1)
{
  {{{
    # M: Model
    pop <- declare_population(
      N = N,
      u_t1 = rnorm(N, 0, sqrt(1 - rho)),
      u_t2 = rnorm(N, 0, sqrt(1 - rho)),
      u_i = rnorm(N, 0, sqrt(rho))
    )
    pos_t1 <- declare_potential_outcomes(Y_t1 ~ u_i + u_t1)
    pos_t2 <- declare_potential_outcomes(Y_t2 ~ ate * Z + u_i + u_t2)
    report <- declare_assignment(m = round(N * (1 - attrition_rate)),
                                 assignment_variable = R)
    # I: Inquiry
    estimand <- declare_estimand(ATE = mean(Y_t2_Z_1 - Y_t2_Z_0))
    # D: Data Strategy
    assignment <-
      declare_assignment(m = round(N / 2), assignment_variable = Z)
    # A: Answer Strategy
    pretest_lhs <- declare_estimator((Y_t2 - Y_t1) ~ Z,
                                     model = lm_robust,
                                     estimand = estimand,
                                     subset = R == 1,
                                     label = "Change score"
    )
    pretest_rhs <- declare_estimator(
      Y_t2 ~ Z + Y_t1,
      model = lm_robust,
      estimand = estimand,
      subset = R == 1,
      label = "Condition on pretest"
    )
    posttest_only <- declare_estimator(
      Y_t2 ~ Z,
      model = lm_robust,
      estimand = estimand,
      label = "Posttest only"
    )
    # Design
    pretest_posttest_design <- declare_design(
      pop,
      pos_t1,
      pos_t2,
      estimand,
      assignment,
      report,
      declare_reveal(Y_t1),
      declare_reveal(Y_t2),
      pretest_lhs,
      pretest_rhs,
      posttest_only
    )
  }}}
  
  attr(pretest_posttest_design, "code") <- 
    construct_design_code(pretest_posttest_designer, match.call.defaults())
  
  pretest_posttest_design
}
attr(pretest_posttest_designer, "shiny_arguments") <- list(
  N = c(100, 50, 1000),
  ate = c(.25,0,.5),
  rho = c(.25,0,.5),
  attrition_rate = c(0,.1,.2)
)
attr(pretest_posttest_designer, "tips") <- c(
  N = "Size of sample",
  ate = "Average treatment effect",
  rho = "Correlation in outcomes between pre- and post-test",
  attrition_rate = "Proportion of respondents lost when using pre-test data"
)
attr(pretest_posttest_designer, "description") <- "
<p> A pretest-posttest design with sample of size <code>N</code>, average treatment effect of size <code>ate</code>, 
    and correlation between pre- and post-test outcomes equal to  <code>rho</code>. The proportion of pre-test respondents  
   missing at random from  the post-test follow-up can be set using  <code>attrition_rate</code>.
"





#' A pretest-posttest design
#'
#' Default design created with  \code{\link{pretest_posttest_designer}}
#' 
#' @seealso \code{\link{pretest_posttest_designer}} 
#' @format A design object 
"pretest_posttest_design"











