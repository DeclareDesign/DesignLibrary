#' Create a pretest-posttest design
#'
#' Produces a design in which an outcome Y is observed pre- and post-treatment.
#' The design allows for individual post-treatment outcomes to be correlated with pre-treatment outcomes
#' and for at-random missingness in the observation of post-treatment outcomes. 
#' @details 
#' 
#' See \href{https://declaredesign.org/library/articles/pretest_posttest.html}{vignette online}.
#' 
#' @param N An integer. Size of sample.
#' @param ate A number. Average treatment effect.
#' @param sd_1 Nonnegative number. Standard deviation of period 1 shocks.
#' @param sd_2 Nonnegative number. Standard deviation of period 2 shocks.
#' @param rho A number in [-1,1]. Correlation in outcomes between pre- and post-test.
#' @param attrition_rate A number in [0,1]. Proportion of respondents in pre-test data that appear in post-test data.
#' @param design_name A character vector. Name of design. This is the label of the design object returned by \code{get_design_code()}. Must be provided without spacing.
#' @param fixed A character vector. Names of arguments to be fixed in design. \code{design_name} is always fixed.
#' @return A pretest-posttest design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment
#' @concept difference-in-differences
#' @concept baseline
#' @importFrom DeclareDesign declare_assignment declare_estimand declare_estimator declare_population declare_potential_outcomes declare_reveal declare_step
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
                                      design_name = "pretest_posttest_design",
                                      fixed = NULL)
{
  if(rho < -1 || rho > 1) stop("'rho' must be a value in [-1, 1]")
  if(any(sd_1 < 0, sd_2 < 0)) stop("'sd_1' and 'sd_2' must be nonnegative")
  if(attrition_rate < 0 || attrition_rate > 1) stop("'attrition_rate' must be in [0,1]")
  if(grepl(" ", design_name, fixed = TRUE)) "`design_name` may not contain any spaces."
  argument_names <- names(match.call.defaults(envir = parent.frame()))[-1]
  fixed_wrong <- fixed[!fixed %in% argument_names]
  if(length(fixed_wrong)!=0) stop(paste0("The following arguments in `fixed` do not match a designer argument:", fixed_wrong)) 
  
  fixed_txt <- fixed_expr(c("N", "ate", "sd_1", "sd_2", "rho", "attrition_rate"))
  for(i in 1:length(fixed_txt)) eval(parse(text = fixed_txt[i]))
  
  population_expr <- expr(declare_population(
    N    = !!N_,
    u_t1 = rnorm(!!N_)*!!sd_1_,
    u_t2 = rnorm(!!N_, rho * scale(u_t1), sqrt(1 - (!!rho)^2))*!!sd_2_,
    Y_t1 = u_t1
  ))
  potential_outcomes_expr <- expr(declare_potential_outcomes(Y_t2 ~ u_t2 + !!ate_ * Z))
  report_expr <- expr(declare_assignment(prob = 1 - !!attrition_rate_,
                                         assignment_variable = R))
  {{{
    # M: Model
    population <- eval_bare(population_expr)
    potential_outcomes <- eval_bare(potential_outcomes_expr)
    
    # I: Inquiry
    estimand <- declare_estimand(ATE = mean(Y_t2_Z_1 - Y_t2_Z_0))
    
    # D: Data Strategy
    assignment <- declare_assignment()
    report     <- eval_bare(report_expr)
    reveal_t2 <- declare_reveal(Y_t2) 
    manipulation <- declare_step(difference = (Y_t2 - Y_t1), handler = fabricate)  
    
    # A: Answer Strategy
    pretest_lhs <- declare_estimator(
      difference ~ Z,
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
    pretest_posttest_design <- population + potential_outcomes + estimand + 
      assignment + reveal_t2 + report + manipulation +
      pretest_lhs + pretest_rhs + posttest_only
  }}}
  
  design_code <- construct_design_code(pretest_posttest_designer, match.call.defaults(),
                                       arguments_as_values = TRUE,
                                       exclude_args = union(c("fixed", "design_name"), fixed))
  design_code <- sub_expr_text(design_code, population_expr, potential_outcomes_expr,
                               report_expr)
  design_code <- gsub("pretest_posttest_design <-", paste0(design_name, " <-"), design_code)
  attr(pretest_posttest_design, "code") <- design_code
  pretest_posttest_design
}
attr(pretest_posttest_designer, "definitions") <- data.frame(
  names = c("N",  "ate",  "sd_1",  "sd_2",  "rho",  "attrition_rate", "design_name", "fixed"),
  class = c("integer", rep("numeric", 5), rep("character", 2)),
  min = c(2, -Inf, 0, 0, -1, 0, NA, NA),
  max = c(Inf, Inf, Inf, Inf, 1, 1, NA, NA)
)
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
    and correlation between pre- and post-test outcomes equal to <code>rho</code>. The proportion of pre-test respondents  
   missing at random from the post-test follow-up can be set using <code>attrition_rate</code>.
"

