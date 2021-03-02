#' Create a design with multiple experimental arms
#'
#' Creates a design with \code{m_arms} experimental arms, each assigned with equal probability.
#' 
#' @details 
#' 
#' See \href{https://declaredesign.org/r/designlibrary/articles/multi_arm.html}{vignette online}.
#' 
#' @param N An integer. Sample size.
#' @param m_arms An integer. Number of arms.
#' @param outcome_means A numeric vector of length \code{m_arms}.  Average outcome in each arm.
#' @param sd_i A nonnegative scalar. Standard deviation of individual-level shock (common across arms).
#' @param outcome_sds A nonnegative numeric vector of length \code{m_arms}. Standard deviations for condition-level shocks.
#' @param conditions A vector of length \code{m_arms}. The names of each arm. It can be given as numeric or character class (without blank spaces). 
#' @param args_to_fix A character vector. Names of arguments to be args_to_fix in design. By default, \code{m_arms} and \code{conditions} are always args_to_fix.
#' @return A function that returns a design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment
#' @concept multiarm trial
#' @importFrom DeclareDesign declare_assignment declare_inquiries declare_estimator declare_population declare_potential_outcomes declare_reveal
#' @importFrom fabricatr fabricate 
#' @importFrom randomizr conduct_ra 
#' @importFrom estimatr difference_in_means
#' @importFrom rlang eval_bare expr quo_text quos sym
#' @importFrom utils data
#' @export
#' @examples
#'
#' # To make a design using default arguments:
#' design <- multi_arm_designer()
#'
#'
#' # A design with different means and standard deviations in each arm
#' design <- multi_arm_designer(outcome_means = c(0, 0.5, 2), outcome_sds =  c(1, 0.1, 0.5))
#'
# A design with args_to_fix sds and means. N is the sole modifiable argument.
#' design <- multi_arm_designer(N = 80, m_arms = 4, outcome_means = 1:4,
#'                              args_to_fix = c("outcome_means", "outcome_sds"))
#'

multi_arm_designer <- function(N = 30,
                               m_arms = 3,
                               outcome_means = rep(0, m_arms),
                               sd_i = 1,
                               outcome_sds = rep(0, m_arms),
                               conditions = 1:m_arms,
                               args_to_fix = NULL) {
  outcome_sds_ <- outcome_sds 
  outcome_means_ <- outcome_means
  N_ <- N; sd_i_ <- sd_i
  if (m_arms <= 1 || round(m_arms) != m_arms)
    stop("m_arms should be an integer greater than one.")
  if (length(outcome_means) != m_arms ||
      length(outcome_sds) != m_arms ||
      length(conditions) != m_arms)
    stop("outcome_means, outcome_sds and conditions arguments must be of length m_arms.")
  if (sd_i < 0) stop("sd_i should be nonnegative")
  if (any(outcome_sds < 0)) stop("outcome_sds should be nonnegative")
  
  if (!"outcome_sds" %in% args_to_fix) outcome_sds_ <-  sapply(1:m_arms, function(i) expr(outcome_sds[!!i]))
  if (!"outcome_means" %in% args_to_fix) outcome_means_ <-  sapply(1:m_arms, function(i) expr(outcome_means[!!i]))
  if (!"N" %in% args_to_fix) N_ <- expr(N)
  if (!"sd_i" %in% args_to_fix) sd_i_ <- expr(sd_i)
  
  # Create helper vars to be used in design
  errors <- sapply(1:m_arms, function(x) quos(rnorm(!!N_, 0, !!!outcome_sds_[x])))
  error_names <- paste0("u_", 1:m_arms)
  names(errors) <- error_names
  population_expr <- expr(declare_population(N = !!N_, !!!errors, u = rnorm(!!N_)*!!sd_i_))
  
  conditions <- as.character(conditions)
  
  f_Y <- formula(
    paste0("Y ~ ", paste0(
      "(", outcome_means_, " + ", error_names,
      ")*( Z == '", conditions, "')",
      collapse = " + "), "+ u"))
  
  potential_outcomes_expr <-
    expr(
      declare_potential_outcomes(
        formula = !!f_Y,
        conditions = !!conditions,
        assignment_variables = Z
      )
    )
  assignment_expr <-
    expr(
      declare_assignment(
        num_arms = !!m_arms,
        conditions = !!conditions,
        assignment_variable = Z
      )
    )
  
  # Get all unique pairings of potential outcomes
  all_pairs <- expand.grid(condition1 = conditions,
                           condition2 = conditions)
  all_pairs <- all_pairs[all_pairs[, 1] != all_pairs[, 2], ]
  all_pairs <- t(apply(all_pairs, 1, sort, decreasing = T))
  all_pairs <- unique(all_pairs)
  all_pairs <- as.data.frame(all_pairs)
  all_po_pairs <- t(apply(
    X = all_pairs,
    MARGIN = 1,
    FUN = function(x) paste0("Y_Z_", x)
  ))
  estimand_names <- paste0("ate_Y_",all_pairs[,1],"_",all_pairs[,2])
  estimand_list <- mapply(
    FUN = function(x, y){
      quos(mean(!!sym(x) - !!sym(y)))},
    x = all_po_pairs[,1],
    y = all_po_pairs[,2])
  names(estimand_list) <- estimand_names
  estimand_expr <- expr(declare_inquiries(!!!estimand_list))
  
  estimators <- mapply(
    FUN = function(x,y){
      expr(difference_in_means(formula = Y ~ Z, 
                               data = data, 
                               condition1 = !!x, 
                               condition2 = !!y))
    },
    x = as.character(all_pairs[,2]),
    y = as.character(all_pairs[,1])
  )
  
  estimator_labels <- paste0("DIM (Z_", as.character(all_pairs[,1]), " - Z_", as.character(all_pairs[,2]), ")")
  
  names(estimators) <- estimand_names
  estimator_expr <- expr(declare_estimator(
    handler = function(data){
      estimates <- rbind.data.frame(!!!estimators)
      names(estimates)[names(estimates) == "N"] <- "N_DIM"
      estimates$estimator_label <- !!estimator_labels
      estimates$inquiry_label <- rownames(estimates)
      estimates$estimate <- estimates$coefficients
      estimates$term <- NULL
      return(estimates)
    }))
  
  
  {{{
    # M: Model
    population <- eval_bare(population_expr)
    
    potential_outcomes <- eval_bare(potential_outcomes_expr)
    
    # I: Inquiry
    estimand  <- eval_bare(estimand_expr)
    
    # D: Data Strategy
    assignment <- eval_bare(assignment_expr)
    
    reveal_Y <-  declare_reveal(assignment_variables = Z)
    
    # A: Answer Strategy
    estimator <- eval_bare(estimator_expr)
    
    # Design
    multi_arm_design <-
      population + potential_outcomes + assignment + reveal_Y + estimand +  estimator
    
  }}}
  
  
  design_code <-
    construct_design_code(
      multi_arm_designer,
      match.call.defaults(),
      arguments_as_values = TRUE,
      exclude_args = union(c("m_arms", "args_to_fix", "conditions"), args_to_fix))
  
  design_code <- sub_expr_text(design_code, population_expr, estimand_expr,
                               potential_outcomes_expr, assignment_expr,
                               estimator_expr)

  attr(multi_arm_design, "code") <- design_code
  
  # Return design
  return(multi_arm_design)
}

attr(multi_arm_designer, "definitions") <- data.frame(
  names = c("N", "m_arms", "outcome_means", "sd_i", 
            "outcome_sds", "conditions", "args_to_fix"),
  tips  = c("Sample size",
            "Number of arms",
            "Average outcome in each arm",
            "Standard deviation of individual-level shock",
            "Standard deviations for condition-level shocks",
            "Names of each arm",
            "Names of arguments to be args_to_fix"),
  class = c("integer", "integer", rep("numeric", 3), rep("character", 2)),
  vector = c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE),
  min   = c(6, 2, -Inf, 0, 0, NA, NA),
  max   = c(Inf, Inf, Inf, Inf, Inf, NA, NA),
  inspector_min = c(120, 2, 0, 0, 0, NA, NA),
  inspector_step = c(60, 1, rep(.2, 3), NA, NA),
  stringsAsFactors = FALSE
)

attr(multi_arm_designer, "shiny_arguments") <-
  list(N = c(10, 20, 50))

attr(multi_arm_designer,"description") <- "
<p> A design with <code>m_arms</code> experimental arms, each assigned with equal probability."
