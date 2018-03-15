#' @export
blocked_and_clustered_template <- function(
  effect_size = c(.25,seq(0,1,.1)),
  N_blocks = seq(5,50,5),
  clusters_per_block = seq(2,10,1),
  units_per_cluster = seq(5,50,5),
  cluster_icc = seq(from = 0.002, to = .502, by = .1),
  block_sd = c(2,seq(from = .01, to = 2.01, by = .25)))
{
  {
    effect_size <- as.numeric(effect_size[1])
    N_blocks <- as.numeric(N_blocks[1])
    clusters_per_block <- as.numeric(clusters_per_block[1])
    units_per_cluster <- as.numeric(units_per_cluster[1])
    cluster_icc <- as.numeric(cluster_icc[1])
    block_sd <- as.numeric(block_sd[1])
    if(cluster_icc <= 0 | cluster_icc >= 1) stop("cluster_icc must be in (0,1)")
  }
  {{{
    population <- declare_population(
      blocks = add_level(
        N = N_blocks, 
        block_mean = rnorm(n = N,mean = 0,sd = block_sd)
      ),
      clusters = add_level(
        N = clusters_per_block
      ),
      individuals = add_level(
        N = units_per_cluster,
        noise = draw_normal_icc(mean = block_mean, clusters = clusters, ICC = cluster_icc, sd = 1)
      ))
    potential_outcomes <- declare_potential_outcomes(
      Y ~ Z * effect_size + noise
    )
    assignment <- declare_assignment(block_prob = rep(.5,N_blocks),
                                     clusters = clusters,
                                     blocks = blocks)
    estimand <- declare_estimand(ate = mean(Y_Z_1 - Y_Z_0))
    estimator <- declare_estimator(
      Y ~ Z, 
      blocks = blocks, 
      clusters = clusters,
      estimand = estimand,
      label = "Difference in means"
    )
    blocked_and_clustered <-
      declare_design(
        population,
        potential_outcomes,
        assignment,
        estimand, 
        estimator
      )
  }}}
  blocked_and_clustered
}
attr(blocked_and_clustered_template,"tips") <- c(
  effect_size = "Average treatment effect in standard deviations",
  N_blocks = "Number of blocks",
  clusters_per_block = "Number of clusters per block",
  units_per_cluster = "Number of units per cluster",
  cluster_icc = "Intra-cluster correlation coefficient",
  block_sd = "Variance in block-level cluster means"
)
