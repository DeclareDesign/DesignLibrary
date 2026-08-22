---
id: blocked_and_clustered
alias: "18.5"
label: blocked and clustered
category: rdss
keywords: [experiment, causal, blocking, cluster]
description: >
  Blocked and clustered randomized trial. (chapter 18). In this design we
  have to assign in clusters but we improve performance by making blocks
  out of clusters of the same size.
params:
  "ICC": "Intracluster correlation"
  "N_clusters": "Number of clusters"
book_link: https://book.declaredesign.org/library/experimental-causal.html#def-ch18num5
include_in_shiny: true
---

design <-
  declare_parameters(
    ICC = 0.9,
    N_clusters = 10
  ) +
  declare_model(
    cluster =
      add_level(
        N = N_clusters,
        cluster_size = rep(seq(10, 50, 10), 2),
        cluster_shock =
          scale(cluster_size + rnorm(N, sd = 5)) * sqrt(ICC),
        cluster_tau = rnorm(N, sd = sqrt(ICC))
      ),
    individual =
      add_level(
        N = cluster_size,
        individual_shock = rnorm(N, sd = sqrt(1 - ICC)),
        individual_tau = rnorm(N, sd = sqrt(1 - ICC)),
        Y_Z_0 = cluster_shock + individual_shock,
        Y_Z_1 = Y_Z_0 + cluster_tau + individual_tau
      )
  ) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = block_and_cluster_ra(clusters = cluster, blocks = cluster_size)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z,
                    clusters = cluster,
                    inquiry = "ATE")
