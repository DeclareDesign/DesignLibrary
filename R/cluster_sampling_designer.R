#' Create a design for cluster random sampling
#'
#' Builds a cluster sampling design of a population with \code{N_clusters} containing \code{N_subjects_per_cluster}. Estimations sample \code{n_clusters} each comprising \code{n_subjects_per_cluster} units. Outcomes within clusters have ICC approximately equal to \code{ICC}.
#'
#' @details 
#' Key limitations: The design assumes clusters draw with equal probability (rather than, for example, proportionate to size).
#' 
#' @param N_clusters An integer. Total number of clusters in the population.
#' @param N_subjects_per_cluster An integer of vector of integers of length \code{N_clusters}. Total number of subjects per cluster in the population.
#' @param n_clusters An integer. Number of clusters to sample.
#' @param n_subjects_per_cluster An integer. Number of subjects to sample per cluster.
#' @param icc A number in [0,1]. Intra-cluster Correlation Coefficient (ICC). 
#' @return A cluster sampling design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept clusters
#' @concept observational
#' @concept measurement
#' @import DeclareDesign stats utils fabricatr estimatr randomizr
#' @export
#' @examples
#' # To make a design using default arguments:
#' cluster_sampling_design <- cluster_sampling_designer()
#' # A design with varying cluster size
#' cluster_sampling_design <- cluster_sampling_designer(
#'   N_clusters = 10, N_subjects_per_cluster = 3:12, 
#'   n_clusters = 5,  n_subjects_per_cluster = 2)

cluster_sampling_designer <- function(N_clusters = 1000,
                                      N_subjects_per_cluster = 50,
                                      n_clusters = 100,
                                      n_subjects_per_cluster = 10,
                                      icc = 0.2
){
  N <- cluster <- latent <- Y <- u_a <- NULL
  if(n_clusters > N_clusters) stop(paste0("n_clusters sampled must be smaller than the total number of ", N_clusters, " clusters."))
  if(n_subjects_per_cluster > min(N_subjects_per_cluster)) stop(paste0("n_subjects_per_cluster must be smaller than the maximum of ", N_subjects_per_cluster, " subjects per cluster."))
  {{{
    # M: Model
    fixed_pop <-
      declare_population(
        cluster = add_level(N = N_clusters),
        subject = add_level(N = N_subjects_per_cluster,
                            latent = draw_normal_icc(mean = 0, N = N, clusters = cluster, ICC = icc),
                            Y = draw_ordered(x = latent, breaks = qnorm(seq(0, 1, length.out = 8)))
        )
      )()
    
    population <- declare_population(data = fixed_pop)

    # I: Inquiry
    estimand <- declare_estimand(mean(Y), label = "Ybar")
    
    # D: Data Strategy
    stage_1_sampling <- declare_sampling(clusters = cluster, n = n_clusters, 
                                         sampling_variable = "Cluster_Sampling_Prob")
    stage_2_sampling <- declare_sampling(strata = cluster,   n = n_subjects_per_cluster, 
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
  n_clusters = "Number of clusters to sample",
  n_subjects_per_cluster = "Number of subjects per cluster to sample",
  icc = "Intra-cluster Correlation"
)
attr(cluster_sampling_designer, "shiny_arguments") <- list(
  n_clusters = c(100, seq(10, 30, 10)),
  n_subjects_per_cluster = seq(10, 40, 10),
  icc = c(0.2, seq(0.002, .999, by = 0.2))
)
attr(cluster_sampling_designer, "description") <- "
<p> A cluster sampling design that samples <code>n_clusters</code> clusters each comprising 
    <code>n_subjects_per_cluster</code> units. The population comprises <code>N_clusters</code> with <code>N_subjects_per_cluster</code> units each. Outcomes within clusters have ICC approximately equal to 
    <code>ICC</code>. 
"

