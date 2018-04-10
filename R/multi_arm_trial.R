#' @export
multi_arm_template <- function(N = c(30, 100, 500, 1000),
                               N_arms = c(3,seq(2,10,1)),
                               mean_effect = 1:5,
                               sd_effect = seq(0,2,.25))
{
  {
    N <- as.numeric(N[1])
    N_arms <- as.numeric(N_arms[1])
    mean_effect <- as.numeric(mean_effect[1])
    sd_effect <- as.numeric(sd_effect[1])
  }
  {{{
    # Model ------------------------------------------------------------------------
    
    population <- declare_population(N = N, noise = rnorm(N))
    
    effects <- rnorm(N_arms,mean_effect,sd_effect)
    POs <- lapply(effects, function(effect) quo(noise + !!effect))
    PO_names <- paste0("Y_Z_T",1:N_arms)
    names(POs) <- PO_names
    potential_outcomes <- declare_potential_outcomes(!!!POs)
    
    # Inquiry ----------------------------------------------------------------------
    expand_estimands <- expand.grid(first = PO_names,second = PO_names)
    expand_estimands <- expand_estimands[expand_estimands[,1] != expand_estimands[,2],]
    
    all_estimands <- apply(expand_estimands,1,function(PO){
      quo(mean(!! sym(PO[1]) - !! sym(PO[2])))
    })
    names(all_estimands) <- apply(expand_estimands,1,paste,collapse = "_")
    estimands <- declare_estimand(!!!all_estimands)
    estimands(potential_outcomes(population()))
    
    
    # Data Strategy ----------------------------------------------------------------
    assignment <- declare_assignment(num_arms = N_arms)
    
    # Answer Strategy --------------------------------------------------------------
    all_estimators <- apply(expand_estimands,1,function(treatment){
      treatment <- gsub(pattern = "Y_Z_","",treatment)
      quo(
        declare_estimator(Y ~ Z,condition1 = !! treatment[1],
                          condition2 = !! treatment[2])
      )
    })
    names(all_estimators) <- apply(expand_estimands,1,paste,collapse = "_")
    
    # Design -----------------------------------------------------------------------
    design <- declare_design(
      population,
      potential_outcomes,
      estimands,
      assignment,
      declare_reveal(Y),
      !!!all_estimators)
  }}}
  design
}

attr(multi_arm_template, "tips") <- c(
  "N" = "",
  "N_arms" = "",
  "mean_effect" = "",
  "sd_effect" = ""
)

attr(multi_arm_template, "description") <- "
<p> A multi-arm template.

<p> Estimand is bananas.
"

# multi_arm_trial <- function(
#   N=c(1000, 100, 500, 2000),
#   n_arms = c(10, 25, 50, 75, 100),
#   min_effect_size=c(0, -1, 1),
#   max_effect_size=c(0, -1, 1),
#   alpha = c(0.05, 0.001, 0.01, 0.10)
# ){
#   {
#     N               <- as.numeric(N)[1]
#     n_arms          <- as.numeric(n_arms)[1]
#     min_effect_size <- as.numeric(min_effect_size)[1]
#     max_effect_size <- as.numeric(max_effect_size)[1]
#     alpha           <- as.numeric(alpha)[1]
#   }
#   {{{
#     my_fx <- seq(min_effect_size, max_effect_size, length.out = n_arms - 1)
#     my_conditions <- paste0("T", 1:n_arms)
#     my_formula <- formula(paste0("Y ~ ", paste0(my_fx, " * (Z == 'T", 2:n_arms, "')", collapse = " + "), " + noise"))
# 
#     pop <- declare_population(N = N, noise = rnorm(N))
#     pos <- declare_potential_outcomes(formula = my_formula, conditions = my_conditions)
#     assignment <- declare_assignment(num_arms = n_arms)
# 
#     # works
#     estimator <- declare_estimator(Y ~ Z, model = lm_robust, coefficients = TRUE)
# 
#     design <-
#       declare_design(pop, pos, assignment, estimator)
#   }}}
#   design
# }


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
