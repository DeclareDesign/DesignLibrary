#' Create a design for cluster random sampling
#'
#' Builds a cluster sampling design for an ordinal outcome variable for a population with \code{N_blocks} strata, each with \code{N_clusters_in_block} clusters, each of which contains \code{N_i_in_cluster} units. The sampling strategy involves sampling \code{n_clusters_in_block} clusters in each stratum, and then sampling \code{n_i_in_cluster} units in each cluster. Outcomes within clusters have intra-cluster correlation approximately equal to \code{ICC}.
#'
#' @details 
#' Key limitations: The design assumes a fixed number of clusters drawn in each stratum and a fixed number of individuals drawn from each cluster.
#' 
#' See \href{https://declaredesign.org/library/articles/cluster_sampling.html}{vignette online}.
#' 
#' @param N_blocks An integer. Number of blocks (strata). Defaults to 1 for no blocks. 
#' @param N_clusters_in_block An integer or vector of integers of length \code{N_blocks}. Number of clusters in each block in the population.
#' @param N_i_in_cluster An integer or vector of integers of length \code{sum(N_clusters_in_block)}. Number of units per cluster sampled.
#' @param n_clusters_in_block An integer. Number of clusters to sample in each block (stratum).
#' @param n_i_in_cluster An integer. Number of units to sample in each cluster.
#' @param icc A number in [0,1]. Intra-cluster Correlation Coefficient (ICC). 
#' @return A stratified cluster sampling design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept clusters
#' @concept observational
#' @concept measurement
#' @importFrom DeclareDesign declare_estimand declare_estimator declare_population declare_sampling
#' @importFrom fabricatr fabricate add_level draw_normal_icc draw_ordered
#' @importFrom randomizr conduct_ra draw_rs 
#' @importFrom estimatr lm_robust
#' @importFrom stats qnorm
#' @export
#' @examples
#' # To make a design using default arguments:
#' cluster_sampling_design <- cluster_sampling_designer()
#' # A design with varying block size and varying cluster size
#' cluster_sampling_design <- cluster_sampling_designer(
#'   N_blocks = 2, N_clusters_in_block = 6:7, N_i_in_cluster = 3:15, 
#'   n_clusters_in_block = 5,  n_i_in_cluster = 2)

cluster_sampling_designer <- function(N_blocks = 1,
                                      N_clusters_in_block = 1000,
                                      N_i_in_cluster = 50,
                                      n_clusters_in_block = 100,
                                      n_i_in_cluster = 10,
                                      icc = 0.2
){
  if(n_clusters_in_block > min(N_clusters_in_block)) stop(paste0("n_clusters_in_block sampled must be smaller than the total number of ", N_clusters_in_block, " clusters."))
  if(n_i_in_cluster > min(N_i_in_cluster)) stop(paste0("n_i_in_cluster must be smaller than or equal to the minimum of ", N_i_in_cluster, " subjects per cluster."))
  if(icc < 0 || icc > 1) stop("icc must be a number in [0,1]")
  {{{
    # M: Model
    fixed_pop <-
      declare_population(
        block = add_level(N = N_blocks),
        cluster = add_level(N = N_clusters_in_block),
        subject = add_level(N = N_i_in_cluster,
                            latent = draw_normal_icc(mean = 0, N = N, clusters = cluster, ICC = icc),
                            Y = draw_ordered(x = latent, breaks = qnorm(seq(0, 1, length.out = 8)))
        )
      )()
    
    population <- declare_population(data = fixed_pop)
    
    # I: Inquiry
    estimand <- declare_estimand(mean(Y), label = "Ybar")
    
    # D: Data Strategy
    stage_1_sampling <- declare_sampling(strata = block, 
                                         clusters = cluster, n = n_clusters_in_block, 
                                         sampling_variable = "Cluster_Sampling_Prob")
    stage_2_sampling <- declare_sampling(strata = cluster,   n = n_i_in_cluster, 
                                         sampling_variable = "Within_Cluster_Sampling_Prob")
    
    # A: Answer Strategy
    clustered_ses <- declare_estimator(Y ~ 1,
                                       model = lm_robust,
                                       clusters = cluster,
                                       estimand = estimand,
                                       label = "Clustered Standard Errors")

    # Design
    cluster_sampling_design <- population + estimand +
      stage_1_sampling + stage_2_sampling + clustered_ses
  }}}
  
  attr(cluster_sampling_design, "code") <- 
    construct_design_code(cluster_sampling_designer, match.call.defaults())
  
  cluster_sampling_design 
}
attr(cluster_sampling_designer, "tips") <- list(
  n_clusters_in_block = "Number of clusters to sample",
  n_i_in_cluster = "Number of subjects per cluster to sample",
  icc = "Intra-cluster Correlation"
)
attr(cluster_sampling_designer, "shiny_arguments") <- list(
  n_clusters_in_block = c(100, seq(10, 30, 10)),
  n_i_in_cluster = seq(10, 40, 10),
  icc = c(0.2, seq(0.002, .999, by = 0.2))
)
attr(cluster_sampling_designer, "description") <- "
<p> A cluster sampling design that samples <code>n_clusters_in_block</code> clusters from each block, each
comprising  <code>n_i_in_cluster</code> units from a population with  <code>N_blocks</code> with
<code>N_clusters_in_block</code> with <code>N_i_in_cluster</code> units each. Outcomes 
are ordinal and exhibit ICC approximately equal to <code>ICC</code>. 
"

