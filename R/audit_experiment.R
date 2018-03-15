

audit_template <- function(N = c(100, 500, 1000)) {
  {
    N <- as.numeric(N[1])
  }
  {
    {
      {
        population <-
          declare_population(
            N = N,
            discrimination = rnorm(N),
            type = draw_ordered(
              discrimination,
              breaks = c(-Inf, 0, 0.5, 2, Inf),
              break_labels = c("A", "B", "C", "D")
            )
          )
        
        potential_outcomes <-
          declare_potential_outcomes(
            R_Z_0 = as.numeric(type %in% c("A", "C")),
            R_Z_1 = as.numeric(type %in% c("A", "B")),
            Y_Z_0 = ifelse(
              type %in% c("A", "C"),
              draw_binary(latent = discrimination, link = "probit"),
              NA
            ),
            Y_Z_1 = ifelse(
              type %in% c("A", "B"),
              draw_binary(latent = discrimination + 0.25, link = "probit"),
              NA
            ),
            Ystar_Z_0 = ifelse(type %in% c("A", "C"), Y_Z_0, 0),
            Ystar_Z_1 = ifelse(type %in% c("A", "B"), Y_Z_1, 0)
          )
        
        # Inquiry -----------------------------------------------------------------
        estimand_1 <-
          declare_estimand(mean(R_Z_1 - R_Z_0), label = "ATE on Response")
        estimand_2 <-
          declare_estimand(mean(Y_Z_1[type == "A"] - Y_Z_0[type == "A"]), label = "ATE on Tone")
        estimand_3 <-
          declare_estimand(mean(Ystar_Z_1 - Ystar_Z_0), label = "ATE on Tone (Alternative)")
        
        # Data Strategy -----------------------------------------------------------
        assignment <- declare_assignment(prob = 0.5)
        
        # Answer Strategy ---------------------------------------------------------
        estimator_1 <-
          declare_estimator(R ~ Z, estimand = estimand_1, label = "ATE on Response")
        estimator_2 <-
          declare_estimator(Y ~ Z, estimand = estimand_2, label = "ATE on Tone")
        estimator_3 <-
          declare_estimator(Ystar ~ Z, estimand = estimand_3, label = "ATE on Tone (Alternative)")
        
        # Design ------------------------------------------------------------------
        design <- declare_design(
          population,
          potential_outcomes,
          assignment,
          estimand_1,
          estimand_2,
          estimand_3,
          declare_reveal(outcome_variables = c("R", "Y", "Ystar")),
          estimator_1,
          estimator_2,
          estimator_3
        )
      }
    }
  }
  design
}
attr(audit_template, "tips") <- c(N = "Size of sample")
