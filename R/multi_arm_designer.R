#' Create a m_arm design
#'
#' This designer creates a \code{m_arm} arm design with equal assignment probabilities accross arms.
#'
#' @param N An integer. Sample size.
#' @param m_arm An integer. Number of treatment arms.
#' @param means A vector of size \code{n_arm}.  Average outcome in each treatment arm.
#' @param sd A double. Standard deviation for all treatment arms.
#' @param fixed A list. List of arguments to be fixed in design.
#' @return A function that returns a design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment 
#' @concept multi-trial
#' @export
#' @examples
#' # To make a design using default arguments:
#'  \dontrun{
#' design <- multi_arm_designer()
#' get_estimates(design)
#' 
#' 
#' # A design with different mean and sd in each arm
#' design <- multi_arm_designer(means = c(0, 0.5, 2), sd =  c(1, 0.1, 0.5))
#' diagnose_design(design)
#' 
# A design with fixed sds and means. N is the sole modifiable argument. 
#' design <- multi_arm_designer(N = 80, m_arm = 4, means = 1:4, fixed = list(m_arm = 4, sds = rep(1, 4), means = 1:4))
#' cat(get_design_code(design), sep = '\n')
#' diagnose_design(design)
#' 
#' If 'means' or 'sds' are defined in fixed list the same definition has to be  given in the arguments list.
#' multi_arm_designer(N = 20, fixed = list(means = 1:3))
#' multi_arm_designer(N = 20, fixed = list(sds = 1:3))
#' }

multi_arm_designer <- function(
  N = 30, 
  m_arm = 3, 
  means = rep(0, m_arm),
  sds = rep(1, m_arm),   
  fixed = NULL
){
  # Housekeeping
  if("means" %in% names(fixed)) if(!identical(means, fixed$means)) stop(paste("Conflicting definitions of 'means' in argument list (possibly the default)--(", paste(means, collapse = ","), ")--and in the fixed list--(", paste(fixed$means, collapse = ","), "). If 'means' is defined in fixed list the same definition has to be  given in the arguments list."))
  if("sds" %in% names(fixed)) if(!identical(sds, fixed$sds)) stop(paste("Conflicting definitions of 'sds' in argument list (possibly the default)--(", paste(sds, collapse = ","), ")--and in the fixed list--(", paste(fixed$sds, collapse = ","), "). If 'sds' is defined in fixed list the same definition has to be  given in the arguments list."))
  if(length(means) != m_arm || length(sds) != m_arm) stop("`means' and `sds` arguments must be the of length m_arm .")
  if(m_arm <= 0 || round(m_arm)!=m_arm) stop("`m_arm' should be a positive integer.")
  if(any(sds<=0)) stop("`sds' should be positive.")
  
  # Create list for substitution
  conds = paste0("T", 1:m_arm)
  
  U <- paste0(" population <- declare_population(N = N, ",  
                        paste0("u_", 1:m_arm, " = rnorm(n = N, sd = ",  sds, ")", collapse = ", "), ")")
  
                
  f_Y = formula(paste0(
    "Y ~ ", paste0("(", means, " + u_", 1:m_arm, ")*( Z == 'T", 1:m_arm, "')", collapse = " + "))
  )
  
  
  Q <- paste0(" estimand <- declare_estimand('(Intercept)' = mean(Y_Z_T1), ",
                     paste0("ZT", 2:m_arm, " = mean(Y_Z_T", 2:m_arm, " - Y_Z_T1)", collapse = ", "), ", coefficients = TRUE)")
  
  fixes <- list(conds = conds, f_Y = f_Y, U = U, Q = Q)
  
  fixes <- c(fixes, fixed)
  
  # Design code with arguments in list substituted
  design_code <-  substitute({
    
    "# M: Model"
    U
    potential_outcomes <- declare_potential_outcomes(formula = f_Y, conditions = conds)
    
    "# I: Inquiry"
    Q
    
    "# D: Data"
    assignment <- declare_assignment(num_arms = m_arm)
    reveal <- declare_reveal()
    
    "# A: Answer" 
    estimator <- declare_estimator(Y ~ Z, model = lm_robust, coefficients = TRUE)
    
    
    "# Design"
    multi_arm_design <- population + potential_outcomes + assignment + reveal + estimand +  estimator
    
  }, fixes)
  
  # Run the design code and create the design
  design_code <- clean_code(paste(design_code) )
  
  eval(parse(text = (design_code)))
  
  

  #  Add  code plus argments as attributes
  attr( multi_arm_design, "code") <- 
    paste0(c(return_args(match.call.defaults(), fixes), design_code))
  
  # Return design
  return( multi_arm_design)
}

attr( multi_arm_designer, "shiny_arguments") <- list(N = c(10, 20, 50), ate = c(0, .5)) 

attr( multi_arm_designer, "tips") <-
  list(
    N = "Sample Size",
    m_arm = "Number of arms",
    means = "The average treatment effects",
    sds  = "The standard deviation in each treatment arm"
  )



