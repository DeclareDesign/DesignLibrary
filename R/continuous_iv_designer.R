#' Create a continuous instrumental variables design

library(rlang)
library(DesignLibrary)

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
  
  
  potentials_expr <- expr(declare_potential_outcomes(handler = function(data){
    data[,!!treatment_name] = fx(data, data[,!!instrument_name])
    data[,!!outcome_name] = fy(data, data[,!!treatment_name], data[,!!instrument_name])
    data
  }))
  
  estimand_expr <- expr(estimand_fn <- function(data){
    categ <- function(x, breaks = n_steps){
      step <- (max(x) - min(x))/breaks
      min(x) + (0:breaks)*step
    }
    
    catX <- categ(data[,!!treatment_name])
    
    omegas <- sapply(catX[-1], function(x) mean((x <= data$X)*(data$Z - mean(data$Z))))
    omega  <- omegas/sum(omegas)
    
    g_prime <- sapply(2:length(catX), function(i) mean((fy(data, catX[i]) - fy(data, catX[i-1]))))
    late <- sum(g_prime*omega)
    
    z <- data[[!!instrument_name]]
    first_stage <- mean((fx(data, max(z)) - fx(data, min(z))))/(max(z)-min(z))

    data.frame(estimand_label = c("first_stage", "LATE"),
               estimand = c(first_stage, late),
               stringsAsFactors = FALSE)
  })

  form1 <- as.formula(paste(treatment_name, instrument_name, sep = " ~ "))
  estimator1_expr <- expr(declare_estimator(!!form1,
                                            estimand = c("first_stage"),
                                            model = lm_robust,
                                            label = "d-i-m"))
  
  form2 <- as.formula(paste(outcome_name, instrument_name, sep = " ~ "))
  estimator2_expr <- expr(declare_estimator(!!form2,
                                            estimand = c("LATE"),
                                            model = lm_robust,
                                            label = "lm_robust"))
  
  form3 <- as.formula(paste0(outcome_name, " ~ ", treatment_name, " | ", instrument_name))
  estimator3_expr <- expr(declare_estimator(!!form3,
                                            estimand = c("LATE"),
                                            model = iv_robust,
                                            label = "iv_robust"))
  {{{
    
    # Model
    population <- declare_population(
      N = N,
      Z = rnorm(N),
      u_Y = rnorm(N)/100 * sd_outcome,
      u_i = rnorm(N) + sd_i,
      u_X = rnorm(N) + sd_X
    )
    
    fx <- function(data, Z) a_X + b_X*Z + data$u_X + data$u_i
    fy <- function(data, X, z = 0) a_Y + b_Y*X^c_Y + d_Y*z + data$u_Y + data$u_i
    
    potentials <- eval_bare(potentials_expr)
    
    # I: Inquiry
    estimand_fn <- eval_bare(estimand_expr)
    estimand <- declare_estimand(handler = estimand_fn)
    
    # Answer Strategy
    estimator_1 <- eval_bare(estimator1_expr)

    estimator_2 <- eval_bare(estimator2_expr)
    
    estimator_3 <- eval_bare(estimator3_expr)
    
    continuous_iv_design <- population + potentials +
      estimand + estimator_1 +
      estimator_2 + estimator_3
    
  }}}
  
  design_code <- construct_design_code(continuous_iv_designer,
                                       match.call.defaults(),
                                       arguments_as_values = TRUE,
                                       exclude_args = c("outcome_name", "treatment_name", "instrument_name", fixed, "fixed"))
  
  design_code <-
    gsub("eval_bare\\(potentials_expr\\)", quo_text(potentials_expr), design_code)
  design_code <-
    gsub("eval_bare\\(estimand_expr\\)", quo_text(estimand_expr), design_code)
  design_code <-
    gsub("eval_bare\\(estimator2_expr\\)", quo_text(estimator2_expr), design_code)
  design_code <-
    gsub("eval_bare\\(estimator3_expr\\)", quo_text(estimator3_expr), design_code)
  
  attr(continuous_iv_design, "code") <- design_code
  continuous_iv_design
}
