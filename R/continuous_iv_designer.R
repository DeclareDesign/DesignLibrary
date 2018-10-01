#' Create a continuous instrumental variables design
#'
#' Builds a design with one continous instrument, one continuous explanatory variable, and one continuous outcome.
#' 
#' @param N An integer. Sample size.
#' @param a_Y A real number. Constant in Y equation.
#' @param b_Y A real number. Effect of X on Y equation. Assumed constant across types. 
#' @param c_Y A real number. Order of polynomial on X in Y equation.
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
                                   c_Y = 1, 
                                   d_Y = 0,
                                   # f(z) = a_X + b_X*Z + uX + ui
                                   a_X = 0,
                                   b_X = 0,
                                   sd_outcome = 1,
                                   sd_i = 1,
                                   sd_X = 1,
                                   n_steps = 10,
                                   outcome_name = c("Y"),
                                   treatment_name = c("X"),
                                   instrument_name = c("Z"),
                                   fixed = NULL
){
  
  if(!"N" %in% fixed) N_ <- expr(N)
  N_ <- N
  
  potentials_expr <- expr(declare_potential_outcomes(handler = function(data){
    
    # First stage PO function
    data[,!!treatment_name] = fx(data, data[,!!instrument_name])
    
    # Second stage PO function
    data[,!!outcome_name] = fy(data, data[,!!treatment_name], data[,!!instrument_name])
    
    data
    }
    ))
  
  e <- exprs(N = !!N_, !!instrument_name := rnorm(!!N_))

  population_expr <- expr(declare_population(
    !!!e,
    u_Y = rnorm(N) * sd_outcome,
    u_i = rnorm(N) * sd_i,
    u_X = rnorm(N) * sd_X
    ))
  
  estimand_expr <- expr(estimand_fn <- function(data){

    categ <- function(x) min(x) + (0:n_steps)*(max(x) - min(x))/n_steps
  
    catX  <- categ(data[,!!treatment_name])
    
    omegas <- sapply(catX[-1], function(x) {
      mean((x <= data[,!!treatment_name])*(data[,!!instrument_name] - mean(data[,!!instrument_name])))
     })

    omega  <- omegas/sum(omegas)
  
    x <- data[,!!treatment_name]
    z <- data[[!!instrument_name]]
    
    g_prime <- sapply(2:length(catX), function(i) mean((fy(data, catX[i], z) - fy(data, catX[i-1], z))))
    late <- sum(g_prime*omega)
    
    ate <- mean((fy(data, max(x), z) - fy(data, min(x), z))/(max(x) - min(x)))
    first_stage <- mean((fx(data, max(z)) - fx(data, min(z))))/(max(z)-min(z))

    data.frame(estimand_label = c("first_stage", "LATE", "ATE"),
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
                                            estimand = c("LATE", "ATE"),
                                            model = lm_robust,
                                            label = "lm_robust_2"))
  
  form3 <- as.formula(paste0(outcome_name, " ~ ", treatment_name, " | ", instrument_name))
  estimator3_expr <- expr(declare_estimator(!!form3,
                                            estimand = c("LATE","ATE"),
                                            model = iv_robust,
                                            label = "iv_robust"))
  {{{
    
    # Model
    population <- eval_bare(population_expr)
    
    fx <- function(data, Z) a_X + b_X*Z + data$u_X + data$u_i
    fy <- function(data, X, Z = 0) a_Y + b_Y*X^c_Y + d_Y*Z + data$u_Y + data$u_i
    
    potentials <- eval_bare(potentials_expr)
    
    # I: Inquiry
    estimand_fn <- eval_bare(estimand_expr)
    estimand <- declare_estimand(handler = estimand_fn)
    
    # Answer Strategy
    estimator_1 <- eval_bare(estimator1_expr)

    estimator_2 <- eval_bare(estimator2_expr)
    
    estimator_3 <- eval_bare(estimator3_expr)
    
    continuous_iv_design <- population + potentials +
      estimand + estimator_1 + estimator_2 + estimator_3
    
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
