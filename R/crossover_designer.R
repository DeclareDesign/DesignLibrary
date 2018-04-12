#' Create a crossover design
#'
#' Description here
#' 
#' Key limitations: Limitations here.
#' 
#' Note: Note here.
#'
#' @param code Logical. If TRUE, returns the code of a design, otherwise returns a design.
#' @param N An integer. Size of sample.
#' @param a A number. Treatment effect of interest
#' @param b A number. Treatment effect of crossed randomization
#' @param crossover A number. Size of crossover effect
#' @param rho A number in [0,1]. Correlation in errors of outcomes A and B
#' @return A crossover design.
#' @export
#'
#' @examples
#' # To make a design using default arguments:
#' crossover_design <- audit_experiment_designer()
#'
#' # To export DeclareDesign code for a design:
#' crossover_designer(code = TRUE)


crossover_designer <- function(N = 100,
                               a = .5,
                               b = .5,
                               crossover = .1, 
                               rho = .2, 
                               code = FALSE) 
{
  design_code <- function() {
    # Below is grabbed by get_design_code
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
  if (code)
    out <- get_design_code(design_code)
  else
    out <- design_code()
  return(out)
}
attr(crossover_designer, "shiny_arguments") <- list(
  N = c(100, 30, 500, 1000),
  a = seq(from = .5, to = -.5, by = -.1),
  b = seq(from = .5, to = -.5, by = -.1),
  crossover = seq(from = .1, to = -.5, by = -.1),
  rho = c(.2, seq(from = -1, to = 1, by = .1)))
attr(crossover_designer,"tips") <- c(
  N = "Size of sample",
  a = "Treatment effect of interest",
  b = "Treatment effect of crossed randomization",
  crossover = "Size of crossover effect",
  rho = "Correlation in errors of outcomes A and B"
)










