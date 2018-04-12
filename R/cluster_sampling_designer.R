#' Create a design for cluster random sampling
#'
#' Description here
#' 
#' Key limitations: Limitations here.
#' 
#' Note: Note here.
#'
#' @param code Logical. If TRUE, returns the code of a design, otherwise returns a design.
#' @param N_clusters An integer. Total number of clusters in the population.
#' @param N_subjects_per_cluster An integer. Total number of subjects per cluster in the population.
#' @param n_clusters An integer. Number of clusters to sample.
#' @param n_subjects_per_cluster An integer. Number of subjects per cluster to sample.
#' @param icc A real number in [0,1] Intra-cluster Correlation. 
#' @return A cluster sampling design.
#' @export
#'
#' @examples
#' # To make a design using default arguments:
#' cluster_sampling_design <- cluster_sampling_designer()
#'
#' # To export DeclareDesign code for a design:
#' cluster_sampling_designer(code = TRUE)
#'

cluster_sampling_designer <- function(n_clusters = 100,
                                      n_subjects_per_cluster = 10,
                                      icc = 0.2,
                                      N_clusters = 1000,
                                      N_subjects_per_cluster = 50,
                                      code = FALSE)
{
  if(n_clusters > N_clusters) stop(paste0("n_clusters must be smaller than the total number of ", N_clusters, " subjects."))
  if(n_subjects_per_cluster > N_subjects_per_cluster) stop(paste0("n_subjects_per_cluster must be smaller than the maximum of ", N_subjects_per_cluster, " subjects per cluster."))
  design_code <- function() {
    {{{
      # M: Model
      population <-
        declare_population(
          cluster = add_level(N = N_clusters),
          subject = add_level(N = N_subjects_per_cluster,
                              latent_ideology = draw_normal_icc(mean = 0, N = N, clusters = cluster, ICC = icc),
                              Y = draw_ordered(x = latent_ideology, breaks = qnorm(seq(0, 1, length.out = 8)))
          )
        )
      fixed_pop <- population()
      
      # I: Inquiry
      estimand <- declare_estimand(mean(Y), label = "Ybar")
      
      # D: Data Strategy
      stage_1_sampling <- declare_sampling(clusters = cluster, n = n_clusters)
      stage_2_sampling <- declare_sampling(strata = cluster, n = n_subjects_per_cluster)
      
      # A: Answer Strategy
      no_clustering <- declare_estimator(Y ~ 1,
                                         model = lm_robust,
                                         estimand = estimand, 
                                         label = "Standard Errors Not Clustered")
      clustered_ses <- declare_estimator(Y ~ 1,
                                         model = lm_robust,
                                         clusters = cluster,
                                         estimand = estimand,
                                         label = "Clustered Standard Errors")
      
      
      # Design
      cluster_sampling_design <- declare_design(fixed_pop, estimand, stage_1_sampling, stage_2_sampling, no_clustering, clustered_ses)
    }}}
    cluster_sampling_design 
  }
  if (code)
    out <- get_design_code(design_code)
  else
    out <- design_code()
  return(out)
}
attr(cluster_sampling_designer, "tips") <- c(
  n_clusters = "Number of clusters to sample",
  n_subjects_per_cluster = "Number of subjects per cluster to sample",
  icc = "Intra-cluster Correlation"
)
attr(cluster_sampling_designer, "shiny_arguments") <- c(
  n_clusters = c(100, seq(10, 100, 10)),
  n_subjects_per_cluster = seq(10, 40, 10),
  icc = c(0.2, seq(0, 1, by = 0.1))
)





