#'
#' @param N An integer. Sample size.
#' @param n_arms An integer. Number of treatment arms.
#' @param means A vector of size \code{n_arm}.  Average outcome in each treatment arm.
#' @param sd A double. Standard deviation for all treatment arms.
#' @param fixed A list. List of arguments to be fixed in design.
#' @return A function that returns a design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment 
#' @export
#' @examples
#' # To make a design using default arguments:
#'  multi_arm_design <- multi_arm_designer()
#' 

multi_arm_designer <- function(
  N = 30, 
  n_arms = 3, 
  means = rep(0, n_arms),
  sd = .1,   
  fixed = NULL
){
  
  if("means" %in% names(fixed)) if(!identical(means, fixed$means)) stop(paste("Conflicting definitions of 'means' in argument list (possibly the default)--(", paste(means, collapse = ","), ")--and in the fixed list--(", paste(fixed$means, collapse = ","), "). If 'means' is defined in fixed list the same definition has to be  given in the arguments list."))
  
  # Create list for substitution
  conds = paste0("T", 1:n_arms)
  
  f_Y = formula(paste0(
    "Y ~ ", paste0(means, " * (Z == 'T", 1:n_arms, "')", collapse = " + "),  " + u")
  )
  estimand <- paste0(" Q <- declare_estimand('(Intercept)' = mean(Y_Z_T1), ",
                     paste0("ZT", 2:n_arms, " = mean(Y_Z_T", 2:n_arms, " - Y_Z_T1)", collapse = ", "), ", coefficients = TRUE)")
  fixes <- list(conds = conds, f_Y = f_Y, estimand = estimand)
  
  fixes <- c(fixes, fixed)
  
  # Design code with arguments in list substituted
  design_code <-  substitute({
    
    "# M: Model"
    U <- declare_population(N = N, u = sd*rnorm(N))
    Y <- declare_potential_outcomes(formula = f_Y, conditions = conds)
    
    "# I: Inquiry"
    estimand
    
    "# D: Data"
    Z <- declare_assignment(num_arms = n_arms)
    R <- declare_reveal()
    
    "# A: Answer" 
    A <- declare_estimator(Y ~ Z, model = lm_robust, coefficients = TRUE)
    
    
    "# Design"
    multi_arm_design <- U + Y + Z + R + Q +  A
    
  }, fixes)
  
  # Run the design code and create the design
  design_code <- clean_code(paste(design_code) )
  
  eval(parse(text = (design_code)))
  
  
  # Get argument list
  args_text <- function(args, fixes){
    # Get names of arguments   
    arg_names <- names(args[2:(length(args)-1)])
    
    # Exclude any fixed arguments
    if(!is.null(fixes)) arg_names <- arg_names[!(arg_names%in%names(fixes))]
    
    # Format
    sapply(arg_names, function(x) paste0(x, " <- ", deparse(args[[x]])))
  }
  
  #  Add  code plus argments as attributes
  attr( multi_arm_design, "code") <- 
    paste0(c(args_text(match.call.defaults(), fixes), design_code))
  
  # Return design
  return( multi_arm_design)
}

attr( multi_arm_designer, "shiny_arguments") <- list(N = c(10, 20, 50), ate = c(0, .5)) 

attr( multi_arm_designer, "tips") <-
  list(
    N = "Sample Size",
    means = "The average treatment effects",
    n_arms = "Number of arms"
  )



