#' Create a two-arm design with blocks and clusters
#'
#' Builds a two-arm design with blocks and clusters.
#'
#' @details
#' 
#' Units are assigned to treatment using complete block cluster random assignment. Treatment effects can be specified either by providing \code{control_mean} and \code{treatment_mean}
#' or by specifying an \code{ate}. Estimation uses differences in means accounting for blocks and clusters.
#' 
#' Total N is given by \code{N_blocks*N_clusters_in_block*N_i_in_cluster} 
#' 
#' Normal shocks can be specified at the individual, cluster, and block levels. If individual level shocks are not specified and cluster and block 
#' level variances sum to less than 1, then individual level shocks are set such that total variance in outcomes equals 1.
#' 
#' Key limitations: The designer assumes covariance between potential outcomes at individual level only.
#' 
#' @param N_blocks An integer. Number of blocks. Defaults to 1 for no blocks. 
#' @param N_clusters_in_block An integer. Number of clusters in each block. This is the total \code{N} when \code{N_blocks} and \code{N_i_in_cluster} are at default values. 
#' @param N_i_in_cluster An integer. Individuals per cluster. Defaults to 1 for no clusters.
#' @param sd_block A nonnegative number. Standard deviation of block level shocks.
#' @param sd_cluster A nonnegative number. Standard deviation of cluster level shock.
#' @param sd_i_0 A nonnegative number. Standard deviation of individual level shock in control. For small \code{sd_block} and \code{sd_cluster}, \code{sd_i_0} defaults to make total variance = 1.
#' @param sd_i_1 A nonnegative number. Standard deviation of individual level shock in treatment. Defaults to \code{sd_i_0}.
#' @param rho A number in [-1,1]. Correlation in individual shock between potential outcomes for treatment and control.
#' @param prob A number in [0,1]. Treatment assignment probability.
#' @param control_mean A number. Average outcome in control.
#' @param ate A number. Average treatment effect. Alternative to specifying \code{treatment_mean}. Note that ate is an argument for the designer but it does not appear as an argument in design code (design code uses \code{control_mean} and \code{treatment_mean} only.) only.
#' @param treatment_mean A number. Average outcome in treatment. Note: if \code{treatment_mean} is not provided then it is calculated from \code{ate}. If both \code{ate} and  \code{treatment_mean} are provided then only  \code{treatment_mean} is used. 
#' @return A block cluster two-arm design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment 
#' @concept blocking
#' @import DeclareDesign stats utils fabricatr estimatr randomizr
#' @export
#' @examples
#' # Generate a design using default arguments:
#' block_cluster_two_arm_design <- block_cluster_two_arm_designer()
#' 
#'

block_cluster_two_arm_designer <- function(N_blocks = 1,
                                           N_clusters_in_block = 100,
                                           N_i_in_cluster = 1,
                                           sd_block = .5,
                                           sd_cluster = .5,
                                           sd_i_0 = sqrt(max(0, 1 - sd_block^2 - sd_cluster^2)),
                                           sd_i_1 = sd_i_0,
                                           rho = 1,
                                           prob = .5,
                                           control_mean = 0,
                                           ate = 0,
                                           treatment_mean = control_mean + ate
                                           ){  
  N <- u_0 <- Y_Z_1 <- Y_Z_0 <- blocks <- clusters <- NULL
  if(sd_block < 0) stop("sd_block must be nonnegative")
  if(sd_cluster < 0) stop("sd_cluster must be nonnegative")
  if(sd_i_0 < 0) stop("sd_i_0 must be nonnegative")
  if(sd_i_1 < 0) stop("sd_i_1 must be nonnegative")
  if(prob< 0 || prob > 1) stop("prob must be in [0,1]")
  if(rho< -1 || rho > 1) stop("correlation must be in [-1,1]")
  {{{    
    # M: Model
    population <- declare_population(
      blocks   = add_level(
        N = N_blocks,
        u_b = rnorm(N) * sd_block),
      clusters = add_level(
        N = N_clusters_in_block,
        u_c = rnorm(N) * sd_cluster,
        cluster_size = N_i_in_cluster),
      i = add_level(
        N   = N_i_in_cluster,
        u_0 = rnorm(N),
        u_1 = rnorm(n = N, mean = rho * u_0, sd = sqrt(1 - rho^2)))
    )
    
    potentials <- declare_potential_outcomes(
      Y ~ (1 - Z) * (control_mean    + u_0*sd_i_0 + u_b + u_c) + 
          Z *       (treatment_mean  + u_1*sd_i_1 + u_b + u_c) )
    
    # I: Inquiry
    estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
    
    # D: Data Strategy
    assignment <- declare_assignment(prob = prob, blocks = blocks, clusters = clusters)
    reveal     <- declare_reveal()
    
    # A: Answer Strategy
    estimator <- declare_estimator(
      Y ~ Z,
      estimand = estimand,
      model = lm_robust,
      fixed_effects = ~ blocks,
      clusters = clusters
    )
    
    # Design
    block_cluster_two_arm_design <-  population + potentials + estimand + assignment + 
                                     reveal + estimator
  }}}
  
  attr(block_cluster_two_arm_design, "code") <- 
    construct_design_code(block_cluster_two_arm_designer, match.call.defaults(),
                          exclude_args = "ate",
                          arguments_as_values = TRUE)
  
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



