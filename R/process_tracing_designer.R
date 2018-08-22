# p_t1_H = .3, # Probability test 1 passed given H 
# p_t2_H = .8, # Probability test 2 passed given H
# cor_t1t2_H = 0,# Correlation test 1 test 2 given H
# p_t1_not_H = 0,# Probability test 1 passed given not H
# p_t2_not_H = .2, # Probability test 2 passed given not H
# cor_t1t2_not_H = 0, # Correlation test 1 test 2 given not H


process_tracing_designer <- function(
  N = 100,
  prior_H = .5,
  p_t1_H = .3, # Probability test 1 passed given H 
  p_t2_H = .8, # Probability test 2 passed given H
  cor_t1t2_H = 0,# Correlation test 1 test 2 given H
  p_t1_not_H = 0,# Probability test 1 passed given not H
  p_t2_not_H = .2, # Probability test 2 passed given not H
  cor_t1t2_not_H = 0, # Correlation test 1 test 2 given not H
  Label_1 = "Smoking Gun",
  Label_2 = "Straw in the Wind"
){
  
  test_prob = function(p1, p2, rho) {
    r = rho * (p1 * p2 * (1 - p1) * (1 - p2))^.5
    c((1 - p1) * (1 - p2) + r,p2 * (1 - p1) - r, p1 * (1 - p2) - r, p1 * p2 + r)}
  if(min(test_prob(p_t1_H, p_t2_H, cor_t1t2_H)) <0) stop("Correlation coefficient not compatible with probabilities")
  if(min(test_prob(p_t1_not_H, p_t2_not_H, cor_t1t2_not_H)) <0) stop("Correlation coefficient not compatible with probabilities")
  
  {{{
    
    # M: Model
    population <- declare_population(
      N = N,
      causal_process = sample(c('X_causes_Y', 'Y_regardless',
                                'X_causes_not_Y', 'not_Y_regardless'),
                              N,TRUE),
      X = rbinom(N, 1, .7) == 1,
      Y = (X & causal_process == "X_causes_Y") | # 1. X causes Y
        (!X & causal_process == "X_causes_not_Y") | # 2. Not X causes Y
        (causal_process == "Y_regardless")# 3. Y happens irrespective of X
    )
    
    # D: Data Strategy 1
    select_case <- declare_sampling(
      strata = paste(X, Y),
      strata_n = c("X0Y0" = 0, "X0Y1" = 0, "X1Y0" = 0, "X1Y1" = 1))
    
    # I: Inquiry
    estimand <-
      declare_estimand(did_X_cause_Y = causal_process == 'X_causes_Y')
    
    # D: Data Strategy 2
    # Calculate bivariate probabilities given correlation
    joint_prob <- function(p1, p2, rho) {
      r <- rho * (p1 * p2 * (1 - p1) * (1 - p2)) ^ .5
      c(
        p00 = (1 - p1) * (1 - p2) + r,
        p01 = p2 * (1 - p1) - r,
        p10 = p1 * (1 - p2) - r,
        p11 = p1 * p2 + r)}
    joint_prob_H <- joint_prob(p_t1_H, p_t2_H, cor_t1t2_H)
    joint_prob_not_H <- joint_prob(p_t1_not_H, p_t2_not_H, cor_t1t2_not_H)
    
    trace_processes <- declare_step(
      test_results = sample(
        c("00", "01", "10", "11"),1, 
        prob = ifelse(rep(causal_process == "X_causes_Y", 4),
                      joint_prob_H,
                      joint_prob_not_H)),
      t1 = test_results == "10" | test_results == "11",
      t2 = test_results == "01" | test_results == "11",
      handler = fabricate)
    
    # A: Answer Strategy
    bayes_estimator <- function(data, p_H = prior_H, p_evid_H, p_evid_not_H, 
                                label, result) {
      data.frame(
        posterior_H = p_evid_H * p_H / 
          (p_evid_H * p_H + p_evid_not_H * (1 - p_H)),
        estimator_label = label,
        estimand_label = "did_X_cause_Y",
        test_results = result
      )}
    
    no_tests <- declare_estimator(
      handler = bayes_estimator,
      p_evid_H = 1,
      p_evid_not_H = 1,
      label = "No tests (Prior)",
      result = TRUE
    )
    
    smoking_gun <- declare_estimator(
      handler = bayes_estimator,
      p_evid_H = ifelse(data$t1, p_t1_H, 1 - p_t1_H),
      p_evid_not_H = ifelse(data$t1, p_t1_not_H, 1 - p_t1_not_H),
      label = Label_1,
      result = data$t1
    )
    
    straw_in_wind <- declare_estimator(
      handler = bayes_estimator,
      p_evid_H = ifelse(data$t2, p_t2_H, 1 - p_t2_H),
      p_evid_not_H = ifelse(data$t2, p_t2_not_H, 1 - p_t2_not_H),
      label = Label_2,
      result = data$t2
    )
    
    joint_test <- declare_estimator(
      handler = bayes_estimator,
      p_evid_H = joint_prob_H[c("00", "01", "10", "11") %in% data$test_results],
      p_evid_not_H = joint_prob_not_H[c("00", "01", "10", "11") %in% data$test_results],
      label = paste(Label_1, "and", Label_2),
      result = data$test_results
    )
    
    # Design
    process_tracing_design <-
      population + select_case + trace_processes + estimand +
      no_tests + smoking_gun + straw_in_wind + joint_test
    
    process_tracing_design <- set_diagnosands(
      process_tracing_design,
      diagnosands = declare_diagnosands(
        bias = mean(posterior_H - estimand),
        rmse = sqrt(mean((posterior_H - estimand)^2)),
        mean_estimand = mean(estimand),
        mean_posterior = mean(posterior_H),
        sd_posterior = sd(posterior_H),
        keep_defaults = FALSE
      ))
    
  }}}
  
  attr(process_tracing_design, "code") <- 
    DesignLibrary:::construct_design_code(process_tracing_designer, match.call.defaults())
  
  process_tracing_design
}
