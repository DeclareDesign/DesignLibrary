
#' @export
election_forecast_template <- function(N = c(500, 100, 500, 1000),
                                       overreporting_rate = c(0.1, 0.3, 0.5)) {
  {
    N <- as.numeric(N[1])
    overreporting_rate <- as.numeric(overreporting_rate[1])
  }
  {
    {
      {
        population <- declare_population(
          N = N,
          U = rnorm(N),
          voter = draw_binary(
            N = N,
            latent = U,
            link = "probit"
          ),
          left_supporter = draw_binary(
            N = N,
            latent = 1 * U,
            link = "probit"
          ),
          overreporter = draw_binary(N = N, prob = (1 - voter) * overreporting_rate),
          likely_voter = voter + overreporter
        )
        
        # sampling <- declare_sampling(n = 500)
        
        estimand <-
          declare_estimand(true_support = mean(left_supporter[voter == 1]))
        estimator <- declare_estimator(
          left_supporter ~ 1,
          model = lm_robust,
          subset = (likely_voter == 1),
          estimand = estimand
        )
        
        election_forecast <-
          declare_design(population, estimand, estimator)
        
      }
    }
  }
  election_forecast
}
attr(election_forecast_template, "tips") <-
  c(N = "Size of sample",
    overreporting_rate = "Extent of overreporting anticipated turnout")

