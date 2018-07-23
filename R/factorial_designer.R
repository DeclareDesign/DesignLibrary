#' Create a factorial design
#'
#' Description:
#'
#' @param N An integer. Size of population to sample from.
#' @return A factorial design
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept factorial
#' @export
#' @examples
#' # A factorial design using default arguments:
#' factorial_design <- factorial_designer()

factorial_designer <- function(
  N = 100,
  n_arms = 3,
  sd = rep(.1, n_arms),
  #n_conditons = 2 # number of conditions per arm
  #prob = .5 # probability of each arm (should be a scalar with length 2^n_arms)
  means = seq(.1:.5, length.out = 8)#rep(0, n_arms)
){
  if(length(means) != 2^n_arms) stop("`ATEs' argument must be the same as length 
                                    of 2^(n_arms). The order of the scalar should
                                    follow the one returned by expand.grid().")
  
  # Create list for substitution
  # conds <- paste0("T", 1:n_arms)
  
  estimand <- paste0("declare_estimand('(Intercept)' = mean(Y_Z_T1), ",
                     paste0("ZT", 
                            2:n_arms, 
                            " = mean(Y_Z_T", 
                            2:n_arms, 
                            " - Y_Z_T1)",
                            collapse = ", "), ", coefficients = TRUE)")
  
  cond_grid <- expand.grid(rep(list(c(0,1)),n_arms))
  cond_names <- paste0("T", 1:n_arms)
  names(cond_grid) <- cond_names
  
  cond_req <- sapply(1:n_arms, function(x) list(which(cond_grid[,x]==1)))
  
  # c <- sapply(1:n_arms, function(c) ifelse(cond_grid[,c]==1, paste0("T", c, "==1"), paste0("T", c, "==0")))
  # cond_logical <- sapply(1:2^n_arms, function(r) paste0(c[r,], collapse = " & "))
  
  # f_Y = formula(paste0(
  #   "Y ~ ", paste0(means, " * (", cond_logical, ")", collapse = " + "),  " + u")
  # )
  
  f_Y = formula(paste0(
    "Y ~ ", paste0(means, " * (Z ==", 1:2^n_arms, ")", collapse = " + "),  " + u")
  )
  
  estimand <- paste0("Q <- declare_estimand('(Intercept)' = mean(Y_Z_1), ",
                     paste0("ZT", 2:2^n_arms, " = mean(Y_Z_", 2:2^n_arms, " - Y_Z_1)", collapse = ", "), ")")#, ", coefficients = TRUE)")
  
  assignment_given_cond <- paste0("Z2 <- declare_step(fabricate,",
                                  paste0("T", 1:n_arms, " = as.numeric(Z %in% ",
                                         cond_req, ")",
                                         collapse = ","), ")")
  
  estimator_formula <- formula(paste0("Y ~ ", paste(cond_names, collapse = "*")))
    
  fixes <- list(n_arms = n_arms, cond_grid = cond_grid, f_Y = f_Y, assignment_given_cond = assignment_given_cond)#, estimand = estimand)
  fixes <- c(fixes, fixed)
  
  {{{
    
    design_code <- substitute({
    
    # M: Model
    population <- declare_population(N = N,
                            u = rnorm(N,0,.1))

    pos <- declare_potential_outcomes(formula = f_Y, conditions = 1:8)
    
    reveal_Y <- declare_reveal(Y, Z)
    
    # I: Inquiry
    estimand
    
    # D: Data Strategy
    Z <- declare_assignment(conditions = 1:2^n_arms)
    
    assignment_given_cond
    
    # A: Answer Strategy 
    A <- declare_estimator(estimator_formula, model = lm_robust, term = TRUE)
    
    # Design
    factorial_design <- population + pos + Z + Z2 + reveal_Y + Q + A
    }, fixes)
    
  }}}
    
    # Run the design code and create the design
    design_code <- clean_code(paste(design_code))
    
    eval(parse(text = (design_code)))
  
  attr(factorial_design, "code") <- 
    construct_design_code(factorial_designer, match.call.defaults())
  
  factorial_design
}

attr(factorial_designer,"shiny_arguments") <-
  list(

  )

attr(factorial_designer,"tips") <-
  c(N = "Size of population to sample from",
     )

attr(factorial_designer,"description") <- "
<p> A regression discontinuity design with sample from population of size <code>N</code>. 
The average treatment effect local to the cutpoint is equal to <code>tau</code>. 
<p> Polynomial regression of order <code>poly_order</code> is used to estimate tau, within a bandwidth of size
<code>bandwidth</code> around the cutoff situated at <code>cutoff</code> on the running variable.
"


