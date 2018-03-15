

srs_template <- function(n = c(100, 500, 1000)) {
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
            Y = draw_ordered(x = latent_ideology, breaks = qnorm(seq(0, 1, length.out = 8)))
          )
        
        # Inquiry -----------------------------------------------------------------
        estimand <- declare_estimand(mean(Y), label = "Ybar")
        
        
        # Data Strategy -----------------------------------------------------------
        sampling <- declare_sampling(n = n)
        
        # Answer Strategy ---------------------------------------------------------
        estimator <- declare_estimator(Y ~ 1,
                                       model = lm_robust,
                                       estimand = estimand)
        
        # Design ------------------------------------------------------------------
        fixed_pop <- population()
        declare_design(fixed_pop, estimand, sampling, estimator)
      }
    }
  }
}
attr(srs_template, "tips") <- c(
  n = "Size of sample"
)


