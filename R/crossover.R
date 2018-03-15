#' @export
crossover_template <- function(N = c(100, 30, 500, 1000),
                               a = seq(from = .5, to = -.5, by = -.1),
                               b = seq(from = .5, to = -.5, by = -.1),
                               crossover = seq(from = .1, to = -.5, by = -.1),
                               rho = c(.2, seq(from = -1, to = 1, by = .1))
)
{  
  {
    N <- as.numeric(N[1])
    a <- as.numeric(a[1])
    b <- as.numeric(b[1])
    crossover <- as.numeric(crossover[1])
    rho <- as.numeric(rho[1]) 
  }
  {{{
    population <- declare_population(
      N = N, 
      noise = rnorm(N),
      u_a = rnorm(N),
      u_b = rnorm(n = N, mean = rho * u_a, sd = 1 - rho^2)
    )
    potential_outcomes_A <- declare_potential_outcomes(
      YA_Z_T1 = noise,
      YA_Z_T2 = noise + u_a + a,
      YA_Z_T3 = noise + crossover * (u_b + b),
      YA_Z_T4 = noise + u_a + a + crossover * (u_b + b)
      )
    potential_outcomes_B <- declare_potential_outcomes(
      YB_Z_T1 = noise,
      YB_Z_T2 = noise + crossover * (u_a + a),
      YB_Z_T3 = noise + u_b + b,
      YB_Z_T4 = noise + u_b + b + crossover * (u_a + a)
      )
    estimand <- declare_estimand(a = a)
    assignment <- declare_assignment(num_arms = 4)
    estimator_sat <- declare_estimator(YA ~ A + B,
                                       model = lm_robust,
                                       coefficients = "A",
                                       estimand = estimand,
                                       label = "Saturated estimator")
    estimator_direct <- declare_estimator(YA ~ A,
                                       model = lm_robust,
                                       coefficients = "A",
                                       estimand = estimand,
                                       label = "Direct estimator")
    estimator_sur <- declare_estimator(
      handler = tidy_estimator(function(data){
        sur_fit <- systemfit::systemfit(
          formula = list(
            YA ~ A,
            YB ~ B),
          method = "SUR",
          data = data)
        data.frame(
          coefficient = "A",
          est = coef(sur_fit)["eq1_A"],
          se = sqrt(diag(vcov(sur_fit)))["eq1_A"],
          p = summary(sur_fit)$coefficients["eq1_A","Pr(>|t|)"],
          ci_lower = confint(sur_fit)["eq1_A",1],
          ci_upper = confint(sur_fit)["eq1_A",2]
        )}),
      estimand = estimand,
      label = "SUR"
    )
    
    crossover_design <- declare_design(
      population,
      potential_outcomes_A,
      potential_outcomes_B,
      estimand,
      assignment,
      dplyr::mutate(A = as.numeric(Z %in% c("T2", "T4")),
                    B = as.numeric(Z %in% c("T3", "T4"))),
      declare_reveal(YA,Z),
      declare_reveal(YB,Z),
      estimator_sat,
      estimator_direct,
      estimator_sur)
  }}}
  crossover_design
}
attr(crossover_template,"tips") <- c(
  N = "Size of sample",
  a = "Treatment effect of interest",
  b = "Treatment effect of crossed randomization",
  crossover = "Size of crossover effect",
  rho = "Correlation in errors of outcomes A and B"
)










