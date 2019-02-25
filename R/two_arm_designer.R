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
#' @param treatment_name A character vector containing the name of the outcome column. Must be provided without spacing.
#' @param outcome_name A character vector containing the name of the assignment (treatment) column. Must be provided without spacing.
#' @param design_name A character vector. Name of design. This is the label of the design object returned by \code{get_design_code()}. Must be provided without spacing.
#' @param fixed A character vector. Names of arguments to be fixed in design.
#' @return A simple two-arm design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment
#' @importFrom DeclareDesign declare_assignment declare_estimand declare_estimator declare_population declare_potential_outcomes declare_reveal
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
                             design_name = "two_arm_design",
                             treatment_name = "Z",
                             outcome_name = "Y",
                             fixed = NULL
){
  if(treatment_mean != ate + control_mean) warning("`treatment_mean` is not consistent with `ate`+`control_mean`. Value provided in `treatment_mean` will override `ate` value.")
  if(control_sd < 0 ) stop("control_sd must be non-negative")
  if(assignment_prob < 0 || assignment_prob > 1) stop("assignment_prob must be in [0,1]")
  if(abs(rho) > 1) stop("rho must be in [-1,1]")
  # if(treatment_mean != control_mean+ate) warning("`treatment_mean` inconsistent with values of `control_mean` and `ate`. Former will override the latter.")
  if(grepl(" ", design_name, fixed = TRUE)) "`design_name` may not contain any spaces."
  if(grepl(" ", treatment_name, fixed = TRUE)) "`treatment_name` may not contain any spaces."
  if(grepl(" ", design_name, fixed = TRUE)) "`design_name` may not contain any spaces."
  argument_names <- names(match.call.defaults(envir = parent.frame()))[-1]
  fixed_wrong <- fixed[!fixed %in% argument_names]
  if(length(fixed_wrong)!=0) stop(paste0("The following arguments in `fixed` do not match a designer argument:", fixed_wrong)) 
  
  fixed_txt <- fixed_expr(c("N","assignment_prob","control_mean","control_sd",
                            "ate","treatment_mean","treatment_sd","rho"))
  for(i in 1:length(fixed_txt)) eval(parse(text = fixed_txt[i]))
  
  population_expr <- expr(
    declare_population(
      N = !!N_,
      u_0 = rnorm(!!N_),
      u_1 = rnorm(n = !!N_, mean = !!rho_ * u_0, sd = sqrt(1 - (!!rho_)^2)))
  )
  
  potential_expr <- expr(
    declare_potential_outcomes(
      !!parse_expr(outcome_name) ~ (1-!!parse_expr(treatment_name)) * (u_0*!!control_sd_ + !!control_mean_) + 
        !!parse_expr(treatment_name)     * (u_1*!!treatment_sd_ + !!treatment_mean_), assignment_variables = !!treatment_name)
  )
  
  assignment_expr <- expr(declare_assignment(prob = !!assignment_prob_, 
                                             assignment_variable = !!treatment_name))
  
  po_cols <- paste(outcome_name, treatment_name, c(1,0), sep = "_")
  estimand_expr <- expr(declare_estimand(ATE = mean(!!sym(po_cols[1]) - !!sym(po_cols[2]))))
  
  estimator_expr <- expr(declare_estimator(!!parse_expr(paste0(outcome_name, "~",  treatment_name)), estimand = "ATE"))
  
  reveal_expr <- expr(declare_reveal(outcome_variables = !!outcome_name, assignment_variables = !!treatment_name))
  
  {{{
    # M: Model
    population <- eval_bare(population_expr)
    
    potential_outcomes <- eval_bare(potential_expr)
    
    # I: Inquiry
    estimand <- eval_bare(estimand_expr)
    
    # D: Data Strategy
    assignment <- eval_bare(assignment_expr)
    
    reveal_Y    <- eval_bare(reveal_expr)
    
    # A: Answer Strategy
    estimator <- eval_bare(estimator_expr)
    
    # Design
    two_arm_design <- population + potential_outcomes + estimand + assignment + reveal_Y + estimator
  }}}
  
  design_code <- 
    construct_design_code(designer = two_arm_designer, 
                          args = match.call.defaults(), 
                          exclude_args = union(c("ate", "fixed", "design_name",
                                                 "treatment_name", "outcome_name"), fixed),
                          arguments_as_values = TRUE)
  
  design_code <-
    gsub("two_arm_design <-", paste0(design_name, " <-"), design_code, fixed = TRUE)
  
  design_code <- sub_expr_text(design_code, population_expr, potential_expr,
                               assignment_expr, estimator_expr, estimand_expr,
                               reveal_expr)
  
  attr(two_arm_design, "code") <- design_code
  
  two_arm_design
}

attr(two_arm_designer, "definitions") <- data.frame(
  names = c("N", "assignment_prob", "control_mean", "control_sd", 
            "ate", "treatment_mean", "treatment_sd", "rho"),
  tips  = c("Sample size",
            "Probability of assignment to treatment",
            "Average outcome in control",
            "Standard deviation in control",
            "Average treatment effect",
            "Average outcome in treatment",
            "Standard deviation in treatment",
            "Correlation between treatment and control outcomes"),
  class = c("integer", rep("numeric", 7)),
  vector = c(rep(FALSE, 8)),
  min   = c(4, 0, -Inf, 0, -Inf, -Inf, 0, -1),
  max   = c(Inf, 1, Inf, Inf, Inf, Inf, Inf, 1),
  inspector_min = c(100, rep(0, 6), -1),
  inspector_step = c(50, rep(.2, 7)),
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


