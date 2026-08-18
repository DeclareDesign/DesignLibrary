---
id: block_cluster_two_arm
label: blocked and clustered two-arm trial
category: template
keywords: [experiment, causal, blocking, cluster]
description: >
  Two-arm trial in which units are nested in clusters and clusters in
  blocks, assignment is at the cluster level and blocked, and the outcome
  carries a block-level, a cluster-level and an individual-level shock.
  Estimation uses block fixed effects with cluster-robust standard errors.
  The default shocks split the outcome variance equally across the three
  levels, an intracluster correlation of two thirds.
  Flattened from DesignLibrary::block_cluster_two_arm_designer.
params:
  "N_blocks": "Number of blocks"
  "N_clusters_in_block": "Number of clusters in each block"
  "N_i_in_cluster": "Number of units in each cluster"
  "sd_block": "Standard deviation of the block-level shock"
  "sd_cluster": "Standard deviation of the cluster-level shock"
  "sd_i": "Standard deviation of the individual-level shock"
  "ate": "Average treatment effect"
  "assignment_prob": "Probability a cluster is assigned to treatment"
include_in_shiny: true
---

N_blocks <- 20
N_clusters_in_block <- 4
N_i_in_cluster <- 10
sd_block <- 0.577
sd_cluster <- 0.577
sd_i <- 0.577
ate <- 0.2
assignment_prob <- 0.5

design <-
  declare_model(
    blocks = add_level(
      N = N_blocks,
      u_b = rnorm(N) * sd_block
    ),
    clusters = add_level(
      N = N_clusters_in_block,
      u_c = rnorm(N) * sd_cluster
    ),
    i = add_level(
      N = N_i_in_cluster,
      u_i = rnorm(N) * sd_i,
      potential_outcomes(Y ~ ate * Z + u_b + u_c + u_i)
    )
  ) +

  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +

  declare_assignment(
    Z = block_and_cluster_ra(blocks = blocks, clusters = clusters,
                             prob = assignment_prob)
  ) +

  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +

  declare_estimator(Y ~ Z, .method = lm_robust,
                    fixed_effects = ~ blocks, clusters = clusters,
                    inquiry = "ATE")
