---
id: village_campaign
alias: "12.1a"
label: village campaign
category: rdss
description: >
  12.1a (chapter 12).
params:
  "citizens_per_village": "Citizens sampled per village"
  "n_villages": "Number of villages in the sample"
  "se_type": "Standard error type for the cluster-robust estimator"
include_in_shiny: false
---

design  <- 
  declare_parameters(
    n_villages = 192,
    citizens_per_village = 48,
    se_type = "stata"
  ) +
  declare_model(
    villages = add_level(N = 660, U_village = rnorm(N, sd = 0.1)),
    citizens = add_level(
      N = 100,
      U_citizen = rnorm(N),
      potential_outcomes(
        Y ~ pnorm(
          U_citizen + U_village +
            0.10 * (Z == "personal") +
            0.15 * (Z == "social")),
        conditions = list(Z = c("neutral", "personal", "social"))
      )
    )
  ) + 
  declare_inquiry(
    ATE_personal = mean(Y_Z_personal - Y_Z_neutral),
    ATE_social = mean(Y_Z_social - Y_Z_neutral)
  ) +
  declare_sampling(
    S_village = cluster_rs(clusters = villages, n = n_villages),
    filter = S_village == 1) +
  declare_sampling(
    S_citizen = strata_rs(strata = villages, n = citizens_per_village),
    filter = S_citizen == 1) +
  declare_assignment(
    Z = cluster_ra(
      clusters = villages, 
      conditions = c("neutral", "personal", "social"),
      prob_each = c(0.250, 0.375, 0.375))) + 
  declare_measurement(
    Y_latent = reveal_outcomes(Y ~ Z),
    Y_observed = rbinom(N, 1, prob = Y_latent)
  ) + 
  declare_estimator(Y_observed ~ Z, term = c("Zpersonal", "Zsocial"), 
                    clusters = villages, 
                    .method = lm_robust,
                    se_type = se_type,
                    inquiry = c("ATE_personal", "ATE_social"))
