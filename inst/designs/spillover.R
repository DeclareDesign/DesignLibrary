---
id: spillover
label: Two-arm trial with spillovers within groups
category: template
keywords: [experiment, causal, spillovers, interference]
description: >
  Units sit in groups, and a unit's outcome depends on the share of its group
  that is treated, raised to the power gamma. The inquiry is the average effect
  of treating a unit alone against treating no one; the naive estimator
  compares treated and untreated units after half of all units are treated.
  Flattened from DesignLibrary::spillover_designer.
params:
  "N_groups": "Number of groups"
  "N_i_group": "Number of units in each group"
  "sd_i": "Standard deviation of the individual-level shock"
  "gamma": "Curvature of the outcome in the group's treated share"
include_in_shiny: true
---

dgp <- function(i, Z, G, n, gamma, sd_i) (sum(Z[G == G[i]]) / n[i])^gamma + rnorm(1) * sd_i

design <-
  declare_parameters(
    N_groups = 80,
    N_i_group = 3,
    sd_i = 0.2,
    gamma = 2
  ) +
  declare_model(
    G = add_level(N = N_groups, n = N_i_group),
    i = add_level(N = n, zeros = 0, ones = 1)
  ) +

  declare_inquiry(Treat_1 = mean(
    sapply(1:length(G), function(i) {
      Z_i <- (1:length(G)) == i
      dgp(i, Z_i, G, n, gamma, sd_i) - dgp(i, zeros, G, n, gamma, sd_i)
    })
  )) +

  declare_assignment(Z = complete_ra(N)) +

  declare_measurement(Y = sapply(1:N, function(i) dgp(i, Z, G, n, gamma, sd_i))) +

  declare_estimator(Y ~ Z, inquiry = "Treat_1", .method = lm_robust,
                    label = "naive")
