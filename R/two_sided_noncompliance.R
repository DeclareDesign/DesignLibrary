#' @export
noncompliance_template <- function(N = c(100, 30, 500, 1000),
                                   pr_compliers = c(0.5, seq(.1, .9, .1)),
                                   cace = c(.2, seq(from = -.5, to = .5, by = .1)))
{
  {
    N <- as.numeric(N[1])
    pr_compliers <- as.numeric(pr_compliers[1])
    cace <- as.numeric(cace[1])
  }
  {
    {
      {
        pop <- declare_population(
          N = N,
          type = sample(
            c("Complier", "Never-taker", "Always-taker"),
            size = N,
            prob = c(pr_compliers, (1 - pr_compliers)/2, (1 - pr_compliers)/2),
            replace = TRUE
          ),
          noise = rnorm(N)
        )
        
        pos_D <-
          declare_potential_outcomes(D ~ as.numeric(type == "Always-taker" |
                                                      type == "Complier" &
                                                      Z == 1))
        
        pos_Y <-
          declare_potential_outcomes(
            Y ~ cace * D +
              0.1 * (type == "Complier") - 0.2 * (type == "Never-taker") +
              0.5 * (type == "Always-taker") +
              noise,
            assignment_variables = "D"
          )
        
        assignment <- declare_assignment(prob = 0.5)
        cace_estimand = declare_estimand(CACE = mean(Y_D_1[type == "Complier"] - Y_D_0[type == "Complier"]))
        iv_estimator = declare_estimator(Y ~ D | Z,
                                         model = iv_robust,
                                         estimand = cace_estimand,
                                         label = "Instrumental Variables")
        as_treated_estimator = declare_estimator(Y ~ D,
                                                 model = lm_robust,
                                                 estimand = cace_estimand,
                                                 label = "As Treated OLS")
        noncompliance <-
          declare_design(
            pop,
            pos_D,
            assignment,
            declare_reveal(D, Z),
            pos_Y,
            declare_reveal(Y, D),
            cace_estimand,
            iv_estimator,
            as_treated_estimator
          )
      }
    }
  }
        noncompliance
}
