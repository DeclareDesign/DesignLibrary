---
id: cluster_sampling
label: Two-stage cluster sampling
category: template
keywords: [descriptive, sampling, clusters]
description: >
  A fixed population of blocks, clusters, and individuals with an ordinal
  outcome whose intra-cluster correlation is icc. Clusters are sampled within
  blocks and then individuals within the sampled clusters, and the population
  mean is estimated with standard errors clustered at the first stage.
  Flattened from DesignLibrary::cluster_sampling_designer.
params:
  "N_blocks": "Number of blocks"
  "N_clusters_in_block": "Number of clusters in each block"
  "N_i_in_cluster": "Number of individuals in each cluster"
  "n_clusters_in_block": "Number of clusters sampled in each block"
  "n_i_in_cluster": "Number of individuals sampled in each sampled cluster"
  "icc": "Intra-cluster correlation of the latent outcome, in [0, 1]"
include_in_shiny: true
---

# The population is drawn once when the design is built, as in
# DesignLibrary 0.1, and redrawn when a redesign changes a size or icc.
design <-
  declare_parameters(
    N_blocks = 1,
    N_clusters_in_block = 1000,
    N_i_in_cluster = 50,
    n_clusters_in_block = 100,
    n_i_in_cluster = 10,
    icc = 0.2,
    dataset = fabricate(
      block = add_level(N = N_blocks),
      cluster = add_level(N = N_clusters_in_block),
      subject = add_level(
        N = N_i_in_cluster,
        latent = draw_normal_icc(mean = 0, N = N, clusters = cluster, ICC = icc),
        Y = draw_ordered(x = latent, breaks = qnorm(seq(0, 1, length.out = 8)))
      )
    )
  ) +
  declare_model(data = dataset) +

  declare_inquiry(Ybar = mean(Y)) +

  declare_sampling(
    S1 = strata_and_cluster_rs(strata = block, clusters = cluster,
                               n = n_clusters_in_block),
    filter = S1 == 1
  ) +
  declare_sampling(
    S2 = strata_rs(strata = cluster, n = n_i_in_cluster),
    filter = S2 == 1
  ) +

  declare_estimator(Y ~ 1, .method = lm_robust, clusters = cluster,
                    inquiry = "Ybar", label = "Clustered Standard Errors")
