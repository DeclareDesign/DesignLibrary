#' Create a continuous instrumental variables design
#'
#' Builds a design with one continous instrument, one continuous explanatory variable, and one continuous outcome.
#' 
#' @param N An integer. Sample size.
#' @param a_Y A real number. Constant in Y equation.
#' @param b_Y A real number. Effect of X on Y equation. Assumed constant across types. 
#' @param c_Y A real number. Effect of complier shock on outcomes
#' @param d_Y A real number. Effect of Z on Y.
#' @param a_X A real number. Constant in X equation.
#' @param b_X A real number. Effect of Z on X. 
#' @param sd_outcome A non negative number. Standard deviation on Y.
#' @param sd_i A non negative number. Standard deviation of individual-level shock.
#' @param sd_x A non negative number. Standard deviation on X.
#' @return A continuous instrumental variables design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment
#' @importFrom DeclareDesign declare_estimand declare_estimator declare_population declare_potential_outcomes declare_reveal diagnose_design
#' @importFrom fabricatr fabricate 
#' @importFrom randomizr conduct_ra 
#' @importFrom estimatr tidy iv_robust lm_robust
#' @importFrom stats runif
#' @export
#' 

continuous_iv_designer <- function(N = 500,
                                   # f(x, z) = a_Y + b_Y*x^c_Y + d_Y*z + uY + ui
                                   a_Y = 0,
                                   b_Y = 0,
                                   d_Y = 0,
                                   # f(z) = a_X + b_X*Z + uX + ui
                                   a_X = 0,
                                   b_X = 0,
                                   gamma = 2,
                                   sd_outcome = 1,
                                   sd_i = 1,
                                   sd_X = 1,
                                   sd_X_type = .5,                  # type heterogeneity 
                                   sd_Y_type = .5,                  # type effect heterogeneity
                                   rho_XY_type = .5,                # if positive: compliers have bigger effects
                                   monotonic = TRUE,
                                   n_steps = 10,
                                   outcome_name = c("Y"),
                                   treatment_name = c("X"),
                                   instrument_name = c("Z"),
                                   fixed = NULL,
                                   estimate_LATE = FALSE
){
  
  if(!"N" %in% fixed) N_ <- expr(N)
  N_ <- N
  
  
  e <- exprs(N = !!N_, !!instrument_name := rnorm(N_))

  population_expr <- expr(declare_population(
    !!!e,
    u_X = rnorm(N) * sd_X,
    u_X_type = rnorm(N, mean = b_X, sd = sd_X_type),
    u_X_type = ifelse(monotonic & (u_X_type< 0), 0, u_X_type),
    u_Y = rnorm(N) * sd_outcome,
    u_Y_type = rnorm(n = N, mean = b_Y + rho_XY_type * u_X_type, sd = sqrt(1 - rho_XY_type^2))*sd_Y_type
    ))
  

  potentials_expr <- expr(declare_potential_outcomes(handler = function(data){
    
    # First stage PO function
    data[,!!treatment_name] = fx(data, data[,!!instrument_name])
    
    # Second stage PO function
    data[,!!outcome_name] = fy(data, data[,!!treatment_name], data[,!!instrument_name])
    
    data
  }
  ))
  
  ifelse(estimate_LATE, estimand_lab <- c("LATE", "ATE"), estimand_lab <- c("ATE"))
  estimand_expr <- expr(estimand_fn <- function(data){

    x <- data[[!!treatment_name]]
    z <- data[[!!instrument_name]]
    
    first_stage <- mean((fx(data, max(z)) - fx(data, min(z))))/(max(z)-min(z))
    ate <- mean((fy(data, max(x), z) - fy(data, min(x), z))/(max(x) - min(x)))
    
    # LATE
    categ   <- function(x) min(x) + (0:n_steps)*(max(x) - min(x))/n_steps
    catZ    <- categ(z)
    Delta_Z <- catZ[2] - catZ[1] 
    catX    <- categ(x)
    Delta_X <- catX[2] - catX[1] 
    mu_z    <- mean(z)
    
    
    lambdas <- sapply(2:length(catZ), function(i) {
                 ((mean(fx(data, catZ[i])) - mean(fx(data, catZ[i-1])))/Delta_Z) *
                 mean((catZ[i] <= z)*(z - mu_z))
              })

    lambda  <- lambdas/sum(lambdas)

    beta_z <- sapply(2:length(catZ), function(iz){
                  xx <- fx(data, catZ[iz])
                  mean((fy(data, xx, z) - fy(data, xx-Delta_X, z))/Delta_X)})
                
                      
    late    <- sum(beta_z*lambda)
    
    if(!estimate_LATE) late <- NULL
    
    # Estimands
    data.frame(estimand_label = c("first_stage", !!estimand_lab),
               estimand = c(first_stage, late, ate),
               stringsAsFactors = FALSE)
  })

  
  form1 <- as.formula(paste(treatment_name, instrument_name, sep = " ~ "))
  estimator1_expr <- expr(declare_estimator(!!form1,
                                            estimand = c("first_stage"),
                                            model = lm_robust,
                                            label = "lm_robust_1"))
  
  form2 <- as.formula(paste(outcome_name, instrument_name, sep = " ~ "))
  estimator2_expr <- expr(declare_estimator(!!form2,
                                            estimand = !!estimand_lab,
                                            model = lm_robust,
                                            label = "lm_robust_2"))
  
  form3 <- as.formula(paste0(outcome_name, " ~ ", treatment_name, " | ", instrument_name))
  estimator3_expr <- expr(declare_estimator(!!form3,
                                            estimand = !!estimand_lab,
                                            model = iv_robust,
                                            label = "iv_robust"))
  {{{
    
    # Model
    population <- eval_bare(population_expr)
    
    fx <- function(data, Z)        a_X + data$u_X_type*Z + data$u_X
    fy <- function(data, X, Z = 0) a_Y + data$u_Y_type*X^gamma + d_Y*Z + data$u_Y 
    
    potentials <- eval_bare(potentials_expr)
    
    # I: Inquiry
    estimand_fn <- eval_bare(estimand_expr)
    estimand <- declare_estimand(handler = estimand_fn)
    
    # Answer Strategy
    estimator_1 <- eval_bare(estimator1_expr)

    estimator_2 <- eval_bare(estimator2_expr)
    
    estimator_3 <- eval_bare(estimator3_expr)
    
    continuous_iv_design <- population + potentials + estimand + 
                            estimator_1 + estimator_2 + estimator_3
    
  }}}
  
  design_code <- construct_design_code(continuous_iv_designer,
                                       match.call.defaults(),
                                       arguments_as_values = TRUE,
                                       exclude_args = c("outcome_name", "treatment_name", "instrument_name", fixed, "fixed"))
  
  design_code <-
    gsub("eval_bare\\(population_expr\\)", quo_text(population_expr), design_code)  
  design_code <-
    gsub("eval_bare\\(potentials_expr\\)", quo_text(potentials_expr), design_code)
  design_code <-
    gsub("eval_bare\\(estimand_expr\\)", quo_text(estimand_expr), design_code)
  design_code <-
    gsub("eval_bare\\(estimator1_expr\\)", quo_text(estimator1_expr), design_code)
  design_code <-
    gsub("eval_bare\\(estimator2_expr\\)", quo_text(estimator2_expr), design_code)
  design_code <-
    gsub("eval_bare\\(estimator3_expr\\)", quo_text(estimator3_expr), design_code)
  
  attr(continuous_iv_design, "code") <- design_code
  continuous_iv_design
}
