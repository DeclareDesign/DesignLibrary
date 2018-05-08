#' Create a two arm design with blocks and clusters
#'
#' This designer builds a design with blocks and clusters. Normal shocks can be specified at the 
#' individual, cluster, and block levels. If individual level shocks are not specified and cluster and block 
#' level variances sum to less than 1, then individual level shocks are set such that total variance in outcomes equals 1. 
#' Treatment effects can be specified either by providing \code{control_mean} and \code{treatment_mean}
#' or by specifying an \code{ate}.
#' 
#' Key limitations: The designer assumes constant treatment effects 
#' and no covariance between potential outcomes.
#' 
#' Note: Default arguments produce a design without blocks and clusters and
#' with N determined by \code{N_cluster_in_block}. Units are assigned to treatment using complete block cluster random assignment. 
#' Analysis uses differences in means accounting for blocks and clusters. 
#'
#' @param N_blocks Number of blocks.
#' @param N_clusters_in_block Number of clusters in each block.
#' @param N_i_in_cluster Individuals per block.
#' @param sd_block Standard deviation of block level shocks.
#' @param sd_cluster Standard deviation of cluster level shock.
#' @param sd_i Standard deviation of individual level shock.
#' @param prob A treatment assignment probability.
#' @param control_mean Average outcome in control.
#' @param ate  Average treatment effect.
#' @param treatment_mean Average outcome in treatment.
#' @return A function that returns a design.
#' @export
#'
#' @examples
#' # To make a design using default arguments:
#' block_cluster_two_arm_design <- block_cluster_two_arm_designer()
#'
#'


block_cluster_two_arm_designer <- function(N_blocks = 1,
                                           N_clusters_in_block = 100,
                                           N_i_in_cluster = 1,
                                           sd_block = .2,
                                           sd_cluster = .2,
                                           sd_i = sqrt(max(0, 1 - sd_block^2 - sd_cluster^2)),
                                           prob = .5,
                                           control_mean = 0,
                                           ate = 1,
                                           treatment_mean = control_mean + ate){  
  
  if(sd_block<0) stop("sd_block must be non-negative")
  if(sd_cluster<0) stop("sd_cluster must be non-negative")
  if(sd_i<0) stop("sd_i must be non-negative")
  if(prob<0 | prob>1) stop("prob must be in [0,1]")
  {{{
    # M: Model
    pop <- declare_population(
      blocks   = add_level(
        N = N_blocks,
        u_b = rnorm(N) * sd_block),
      clusters = add_level(
        N = N_clusters_in_block,
        u_c = rnorm(N) * sd_cluster,
        cluster_size = N_i_in_cluster
      ),
      i = add_level(
        N   = N_i_in_cluster,
        u_i = rnorm(N) * sd_i,
        Z0  = control_mean + u_i + u_b + u_c,
        Z1  = treatment_mean  + u_i + u_b + u_c
      )
    )
    
    pos <- declare_potential_outcomes(Y ~ (1 - Z) * Z0 + Z * Z1)
    
    # I: Inquiry
    estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
    
    # D: Data
    assignment <- declare_assignment(prob = prob,
                                     blocks = blocks,
                                     clusters = clusters)
    
    # A: Analysis
    est <- declare_estimator(
      Y ~ Z,
      estimand = estimand,
      model = difference_in_means,
      blocks = blocks,
      clusters = clusters
    )
    
    # Design
    block_cluster_two_arm_design <-  pop / pos / estimand / assignment / 
      declare_reveal() / est
  }}}
  
  attr(block_cluster_two_arm_design, "code") <- 
    construct_design_code(block_cluster_two_arm_designer, match.call.defaults())
  
  block_cluster_two_arm_design
  
}

attr(block_cluster_two_arm_designer, "shiny_arguments") <-
  list(
    N_blocks = c(10, 20, 50),
    N_clusters_in_block = c(2, 4),
    N_i_in_cluster = c(1, 5, 10),
    ate = c(0, .1, .3)
  )

attr(block_cluster_two_arm_designer, "tips") <-
  list(
    N_blocks = "Number of blocks",
    N_clusters_in_block = "Number of clusters in each block",
    N_i_in_cluster = "Number of units in each cluster",
    ate = "The average treatment effect"
  )
attr(block_cluster_two_arm_designer, "description") <- "
<p> A two blocked and clustered experiment <code>N_blocks</code> blocks, 
each containing <code>N_clusters_in_block</code> clusters. Each cluster in turn contains 
<code>N_i_in_cluster</code> individual units. 
<p> Estimand is the <code>ate</code> average interaction effect.
"



#' A two arm design with blocks and clusters
#'
#' Default design created with  \code{\link{block_cluster_two_arm_designer}}
#' 
#' @seealso \code{\link{block_cluster_two_arm_designer}} 
#' @format A design object 
"block_cluster_two_arm_design"



