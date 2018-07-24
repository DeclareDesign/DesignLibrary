#' Create a factorial design
#'
#' A \code{2^k} factorial designer.
#'
#' @param N An integer. Size of sample.
#' @param k An integer. The number of factors in the design.
#' @param means A scalar. Means for each of the \code{2^k} treatment combinations. See `Details` for the correct order of values. 
#' @param sds A scalar. Standard deviations for each of the \code{2^k} treatment combinations. See `Details` for the correct order of values. 
#' @param probs A scalar of length \code{k}. Independent probability of assignment to each treatment. 
#' @return A factorial design.
#' @details 
#' 
#' \code{factorial_designer} creates a factorial design with \code{2^k} treatment combinations resulting from \code{k} factors, each with two conditions each (\code{c(0,1)}). The order of the scalar arguments \code{means} and \code{sds} must follow the one returned by \code{expand.grid(rep(list(c(0,1)), k))}, where each of the columns is a treatment
#' 
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept factorial
#' @export
#' @examples
#' # A factorial design using default arguments
#' factorial_design <- factorial_designer()
#' 
#' # A 2 x 2 x 2 factorial design with unequal probabilities of assignment
#' # to each treatment
#' factorial_design2 <- factorial_designer(k = 3, probs = c(1/2, 1/4, 1/4))
#' 

factorial_designer <- function(
  N = 500,
  k = 2,
  means = seq(0:.5, length.out = 2^k),
  sds = rep(.1, 2^k),
  probs = rep(.5, k),
  fixed = NULL
){
  if(length(means) != 2^k || length(sds) != 2^k) stop("`means' and `sds` arguments must be the same as length of 2^(k).")
  if(length(probs) != k) stop("`probs` must be the same as length of k.")
  if(k <= 0 || !rlang::is_integerish(k)) stop("`k' should be a positive integer.")
  if(any(sds<=0)) stop("`sds' should be positive.")
  if(any(probs <= 0)) stop("`probs' should have positive values only.")
  
  # Create list for substitution
  cond_names <- paste0("T", 1:k)
  cond_list <- rep(list(c(0,1)),k)
  names(cond_list) <- cond_names
  cond_grid <- expand.grid(cond_list)
  
  # Probability each treatment combination
  prob_each <- apply(sapply(1:k, function(k){
    probs[k] * cond_grid[,k] + (1-probs[k]) * (1-cond_grid[,k])
  }), 1, prod)
  
  cond_row <- lapply(1:k, function(x) which(cond_grid[,x]==1))
  
  c <- sapply(1:k, function(c) ifelse(cond_grid[,c]==1, paste0("T", c, "==1"), paste0("T", c, "==0")))
  cond_logical <- sapply(1:2^k, function(r) paste0(c[r,], collapse = " & "))
  
  a <- sapply(1:k, function(c) ifelse(cond_grid[,c]==1, paste0("T", c, "_1"), paste0("T", c, "_0")))
  assignment_string <- sapply(1:2^k, function(r) paste0(a[r,], collapse = "_"))
  
  f_Y = formula(paste0(
    "Y ~ ", paste0(paste0("(", means, " + ", "u_", 1:2^k, ")"), "* (", cond_logical, ")", collapse = " + "),  " + u")
  )
  
  estimand <- paste0("estimands <- declare_estimand('(Intercept)' = mean(Y_", assignment_string[1], "), ",
                     paste0(assignment_string[-1], " = mean(Y_", assignment_string[-1], " - Y_",
                            assignment_string[1], collapse = "), "), "), term = TRUE)")
  
  
  assignment_given_factor <- paste0("assignment <- declare_step(fabricate,",
                                    paste0("T", 1:k, " = as.numeric(Z %in% ",
                                           cond_row, ")",
                                           collapse = ","), ")")
  
  list_u <- sapply(1:2^k, function(x) return(paste0("u_", x, " = rnorm(", N, ", ", means[x], ", ", sds[x], ")")))
  list_u <- paste(list_u, collapse = ", ")
  
  # estimator_formula <- formula(paste0("Y ~ ", paste(cond_names, collapse = "*")))
  pop <- paste0("population <- declare_population(N = ", N, ", u = rnorm(", N, ", 0, .1), ", 
                list_u, ")")
  
  
  fixes <- list(#k = k, cond_grid = cond_grid, 
    pop = pop, #declare_population function
    f_Y = f_Y, cond_list = cond_list, #declare_potential_outcomes function
    cond_names = cond_names, #reveal_outcomes function
    estimand = estimand, #declare_estimands function
    prob_each = prob_each, #declare_assignment function
    assignment_given_factor = assignment_given_factor, #declare_step function
    assignment_string = assignment_string) #declare_estimator function
  
  fixes <- c(fixes, fixed)
  
  design_code <- substitute({
    
    # M: Model
    pop
    
    potential_outcomes <- declare_potential_outcomes(formula = f_Y, conditions = cond_list)
    
    reveal_Y <- declare_reveal(Y, assignment_variables(cond_names))
    
    # I: Inquiry
    estimand
    
    # D: Data Strategy
    assignment_factors <- declare_assignment(conditions = 1:(2^k), prob_each = prob_each)
    
    assignment_given_factor
    
    # A: Answer Strategy 
    estimators <- declare_estimator(Y ~ as.factor(Z), model = lm_robust,
                                    term = TRUE, estimand = assignment_string)
    
    # Design
    factorial_design <- population + potential_outcomes + assignment_factors + 
      assignment + reveal_Y + estimands + estimators
    
  }, fixes)
  
  # Run the design code and create the design
  design_code <- clean_code(paste(design_code))
  
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
  attr(factorial_design, "code") <- 
    paste0(c(args_text(match.call.defaults(), fixes), design_code))
  
  # Return design
  return(factorial_design)
}

attr(factorial_designer,"shiny_arguments") <-
  list(
    N = c(50, 100, 500, 1000),
    k = c(2, 3, 4)
  )

attr(factorial_designer,"tips") <-
  c(N = "Size of sample",
    k = "The number of factors in the design"
  )

attr(factorial_designer,"description") <- "
<p> A <code>2^k</code> factorial design with sample size <code>N</code>
    and <code>k</code> treatment factors.
"