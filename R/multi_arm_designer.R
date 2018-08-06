#' Create a design with multiple experimental arms
#'
#' This designer creates a design \code{m_arms} experimental arms, each assigned with equal probabilities.
#'
#' @param N An integer. Sample size.
#' @param m_arms An integer. Number of arms.
#' @param means A numeric vector of length \code{m_arms}.  Average outcome in each arm.
#' @param sds A nonnegative numeric vector of length \code{m_arms}. Standard deviations for each of the arms.
#' @param conditions A vector of length \code{m_arms}. The names of each arm. It can be numeric or a character without blank spaces. 
#' @param fixed A character vector. Names of arguments to be fixed in design. By default \code{m_arms} and \code{conditions} are always fixed.
#' @return A function that returns a design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment
#' @concept multiarm trial
#' @import DeclareDesign stats utils fabricatr estimatr randomizr rlang
#' @export
#' @examples
#'
#' # To make a design using default arguments:
#' design <- multi_arm_designer()
#'
#'
#' # A design with different mean and sd in each arm
#' design <- multi_arm_designer(means = c(0, 0.5, 2), sd =  c(1, 0.1, 0.5))
#'
# A design with fixed sds and means. N is the sole modifiable argument.
#' design <- multi_arm_designer(N = 80, m_arms = 4, means = 1:4,
#'                              fixed = c("means", "sds"))
#'

multi_arm_designer <- function(N = 30,
                               m_arms = 3,
                               means = rep(0, m_arms),
                               sds = rep(1, m_arms),
                               conditions = 1:m_arms,
                               fixed = NULL) {
  # Housekeeping
  Y_Z_1 <- Z <- NULL
  sds_ <- sds 
  means_ <- means
  N_ <- N
  if (m_arms <= 1 || round(m_arms) != m_arms)
    stop("m_arms should be an integer greater than one.")
  if (length(means) != m_arms ||
      length(sds) != m_arms ||
      length(conditions) != m_arms)
    stop("means, sds and conditions arguments must be of length m_arms.")
  if (any(sds <= 0)) stop("sds should be nonnegative")
  if (!"sds" %in% fixed) sds_ <-  sapply(1:m_arms, function(i) expr(sds[!!i]))
  if (!"means" %in% fixed) means_ <-  sapply(1:m_arms, function(i) expr(means[!!i]))
  if (!"N" %in% fixed) N_ <- expr(N)
  
  # Create helper vars to be used in design
  errors <- sapply(1:m_arms, function(x) quos(rnorm(!!N_, 0, !!!sds_[x])))
  error_names <- paste0("u_", 1:m_arms)
  names(errors) <- error_names
  population_expr <- expr(declare_population(N = !!N_, !!!errors))
  
  conditions <- as.character(conditions)
  
  f_Y <- formula(
    paste0("Y ~ ",paste0(
      "(", means_, " + ", error_names,
      ")*( Z == '", conditions, "')",
      collapse = " + ")))
  
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
  estimand_names <- paste0("ate_",all_po_pairs[,1],"_",all_po_pairs[,2])
  estimand_list <- mapply(
    FUN = function(x, y){
      quos(mean(!!sym(x) - !!sym(y)))},
    x = all_po_pairs[,1],
    y = all_po_pairs[,2])
  names(estimand_list) <- estimand_names
  estimand_expr <- expr(declare_estimands(!!!estimand_list))
  
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
  names(estimators) <- estimand_names
  estimator_expr <- expr(declare_estimator(
    handler = function(data){
      estimates <- rbind.data.frame(!!!estimators)
      estimates$estimator_label <- "DIM"
      estimates$estimand_label <- rownames(estimates)
      estimates$estimate <- estimates$coefficients
      estimates$term <- NULL
      return(estimates)
    }))
  
  
  {{{
    # M: Model
    population <- eval_bare(population_expr)
    
    potentials <- eval_bare(potential_outcomes_expr)
    
    # I: Inquiry
    estimand  <- eval_bare(estimand_expr)
    
    # D: Data Strategy
    assignment <- eval_bare(assignment_expr)
    
    reveal_Y <-  declare_reveal(assignment_variables = Z)
    
    # A: Answer Strategy
    estimator <- eval_bare(estimator_expr)
    
    # Design
    multi_arm_design <-
      population + potentials + assignment + reveal_Y + estimand +  estimator
    
  }}}
  
  
  design_code <-
    construct_design_code(
      multi_arm_designer,
      match.call.defaults(),
      arguments_as_values = TRUE,
      exclude_args = c("m_arms", fixed, "fixed", "conditions")
    )
  
  design_code <-
    gsub("eval_bare\\(population_expr\\)", quo_text(population_expr), design_code)
  design_code <-
    gsub("eval_bare\\(estimand_expr\\)", quo_text(estimand_expr), design_code)
  design_code <-
    gsub("eval_bare\\(potential_outcomes_expr\\)", quo_text(potential_outcomes_expr), design_code)
  design_code <- gsub("eval_bare\\(assignment_expr\\)", quo_text(assignment_expr), design_code)
  design_code <- gsub("eval_bare\\(estimator_expr\\)", quo_text(estimator_expr), design_code)
  
  #  Add  code plus argments as attributes
  attr(multi_arm_design, "code") <- design_code
  
  
  # Return design
  return(multi_arm_design)
}

attr(multi_arm_designer, "shiny_arguments") <-
  list(N = c(10, 20, 50))

attr(multi_arm_designer, "tips") <-
  list(N = "Sample Size")
