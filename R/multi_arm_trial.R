library(DeclareDesign)
library(dplyr)

N <- 1000
n_arms <- 50
min_effect_size <- 0
max_effect_size <- 0
alpha = 0.05

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


test <- diagnose_design(design, diagnosands = declare_diagnosands(mean_est = mean(est)), sims = 100, bootstrap = FALSE)

test$simulations %>%
  group_by(sim_ID) %>%
  filter(coefficient != "(Intercept)") %>%
  summarize(any_significant = any(p < alpha),
            num_significant = sum(p < alpha),
            all_significant = all(p < alpha)) %>%
  summarize(any_significant = mean(any_significant),
            num_significant = mean(num_significant),
            all_significant = mean(all_significant))
