

weighted_sampling_template <- function(n = c(100, 500, 1000)) {
  {
    n <- as.numeric(n[1])
    N <- 5000
    if(n > N) stop(paste0("n must be smaller than the total population of ", N, "subjects."))
  }
  {
    {
      {
        # Model -------------------------------------------------------------------
        population <-
          declare_population(
            N = N,
            latent_ideology = rnorm(N),
            Y = draw_ordered(x = latent_ideology, breaks = qnorm(seq(0, 1, length.out = 8))),
            prob = pnorm(latent_ideology) * n/sum(pnorm(latent_ideology))
          )
        
        # Inquiry -----------------------------------------------------------------
        estimand <- declare_estimand(mean(Y), label = "Ybar")
        
        # Data Strategy -----------------------------------------------------------
        custom_sampling <-
          function(data) {
            data <-
              within(data, {
                S = rbinom(n = N, size = 1, prob = prob)
              })
            return(data[data$S == 1, , drop = FALSE])
          }
        sampling <- declare_sampling(handler = custom_sampling)
        
        # Answer Strategy ---------------------------------------------------------
        unweighted_estimator <- declare_estimator(Y ~ 1,
                                       model = lm_robust,
                                       estimand = estimand, 
                                       label = "Unweighted")
        
        weighted_estimator <- declare_estimator(Y ~ 1,
                                       model = lm_robust,
                                       weights = 1/ prob,
                                       estimand = estimand,
                                       label = "Weighted")
        
        # Design ------------------------------------------------------------------
        fixed_pop <- population()
        declare_design(fixed_pop, estimand, sampling, unweighted_estimator, weighted_estimator)
      }
    }
  }
}
attr(weighted_sampling_template, "tips") <- c(
  n = "Size of sample"
)

test <- weighted_sampling_template(n = 30)

diagnose_design(test)




