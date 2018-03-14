
#' @export
matching_template <- function(N = c(100, 30, 500, 1000))
{
  {
    N <- as.numeric(N[1])
  }
  {
    {
      {
        population <- declare_population(
          N = N,
          X1 = rnorm(N),
          X2 = rnorm(N),
          X3 = rnorm(N)
        )
        
        potential_outcomes <-
          declare_potential_outcomes(formula = Y ~ X1 + X2 + X3 + Z)
        
        assignment <- declare_assignment(
          handler =  function(data) {
            prob <- with(data, pnorm(X1 + X2 + X3))
            data$Z <- rbinom(nrow(data), 1, prob)
            return(data)
          }
        )
        
        att <- declare_estimand(att = mean(Y_Z_1[Z == 1] - Y_Z_0[Z == 1]))
        
        estimator_d_i_m <-
          declare_estimator(Y ~ Z, estimand = estimand, label = "dim")
        
        matching_helper <- function(data) {
          match_out <- with(data, Matching::Match(
            Y = Y,
            Tr = Z,
            X = cbind(X1, X2, X3)
          ))
          return(
            data.frame(
              coefficient = NA,
              est = match_out$est,
              se = NA,
              p = NA,
              ci_lower = NA,
              ci_upper = NA
            )
          )
        }
        
        estimator_m <- declare_estimator(
          handler = tidy_estimator(matching_helper),
          estimand = att,
          label = "matching"
        )
        
        matching <- declare_design(
          population,
          potential_outcomes,
          assignment,
          declare_reveal(Y, Z),
          estimand,
          estimator_d_i_m,
          estimator_m
        )
      }
    }
  }
  matching
}
attr(matching_template, "tips") <- c(N = "Size of sample")
