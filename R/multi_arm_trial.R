
multi_arm_trial <- function(
  N=c(1000, 100, 500, 2000),
  n_arms = c(10, 25, 50, 75, 100),
  min_effect_size=c(0, -1, 1),
  max_effect_size=c(0, -1, 1),
  alpha = c(0.05, 0.001, 0.01, 0.10)
){
  {
    N               <- as.numeric(N)[1]
    n_arms          <- as.numeric(n_arms)[1]
    min_effect_size <- as.numeric(min_effect_size)[1]
    max_effect_size <- as.numeric(max_effect_size)[1]
    alpha           <- as.numeric(alpha)[1]
  }
  {{{
    my_fx <- seq(min_effect_size, max_effect_size, length.out = n_arms - 1)
    my_conditions <- paste0("T", 1:n_arms)
    my_formula <- formula(paste0("Y ~ ", paste0(my_fx, " * (Z == 'T", 2:n_arms, "')", collapse = " + "), " + noise"))

    pop <- declare_population(N = N, noise = rnorm(N))
    pos <- declare_potential_outcomes(formula = my_formula, conditions = my_conditions)
    assignment <- declare_assignment(num_arms = n_arms)

    # works
    estimator <- declare_estimator(Y ~ Z, model = lm_robust, coefficients = TRUE)

    design <-
      declare_design(pop, pos, assignment, estimator)
  }}}
  design
}


# test <- diagnose_design(design, diagnosands = declare_diagnosands(mean_est = mean(est)), sims = 100, bootstrap = FALSE)
#
# test$simulations %>%
#   group_by(sim_ID) %>%
#   filter(coefficient != "(Intercept)") %>%
#   summarize(any_significant = any(p < alpha),
#             num_significant = sum(p < alpha),
#             all_significant = all(p < alpha)) %>%
#   summarize(any_significant = mean(any_significant),
#             num_significant = mean(num_significant),
#             all_significant = mean(all_significant))
