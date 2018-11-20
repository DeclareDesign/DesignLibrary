#' Create a two-arm design with blocks and clusters
#'
#' Builds a two-arm design with blocks and clusters.
#'
#' @details
#' 
#' Units are assigned to treatment using complete block cluster random assignment. Treatment effects can be specified either by providing \code{control_mean} and \code{treatment_mean}
#' or by specifying an \code{ate}. Estimation uses differences in means accounting for blocks and clusters.
#' 
#' In the usual case \code{N} is not provided by the user but is determined by \code{N_blocks}, \code{N_clusters_in_block}, \code{N_i_in_cluster} (when these are integers \code{N} is the product of these three numbers).
#' 
#' Normal shocks can be specified at the individual, cluster, and block levels. If individual level shocks are not specified and cluster and block 
#' level variances sum to less than 1, then individual level shocks are set such that total variance in outcomes equals 1.
#' 
#' Key limitations: The designer assumes covariance between potential outcomes at the individual level only.
#' 
#' See \href{https://declaredesign.org/library/articles/block_cluster_two_arm.html}{vignette online}.
#' 
#' @param N An integer. Total number of units. Usually not specified as \code{N} is determined by \code{N_blocks}, \code{N_clusters_in_block}, and \code{N_i_in_cluster}. If \code{N_blocks}, and  \code{N_clusters_in_block}, and \code{N_i_in_cluster} are specified then \code{N} is overridden. If these are not specified and \code{N} is specified then designer attempts to guess sizes of levels to approximate \code{N}, with preference for a design without blocks or clusters. 
#' @param N_blocks An integer. Number of blocks. Defaults to 1 for no blocks. 
#' @param N_clusters_in_block An integer or vector of integers of length \code{N_blocks}. Number of clusters in each block. This is the total \code{N} when \code{N_blocks} and \code{N_i_in_cluster} are at default values. 
#' @param N_i_in_cluster An integer or vector of integers of length \code{sum(N_clusters_in_block)}. Individuals per cluster. Defaults to 1 for no clusters.
#' @param sd A nonnegative number. Overall standard deviation (combining individual level, cluster level, and block level shocks). Defaults to 1. Overridden if incompatible with other user-specified shocks. 
#' @param sd_block A nonnegative number. Standard deviation of block level shocks.
#' @param sd_cluster A nonnegative number. Standard deviation of cluster level shock.
#' @param sd_i_0 A nonnegative number. Standard deviation of individual level shock in control. If not specified, and when possible given \code{sd_block} and \code{sd_cluster}, \code{sd_i_0} defaults to make total variance = sd.
#' @param sd_i_1 A nonnegative number. Standard deviation of individual level shock in treatment. Defaults to \code{sd_i_0}.
#' @param rho A number in [-1,1]. Correlation in individual shock between potential outcomes for treatment and control.
#' @param assignment_probs A number or vector of numbers in (0,1). Treatment assignment probability for each block (specified in order of \code{N_clusters_in_block}). 
#' @param control_mean A number. Average outcome in control.
#' @param ate A number. Average treatment effect. Alternative to specifying \code{treatment_mean}. Note that \code{ate} is an argument for the designer but it does not appear as an argument in design code (design code uses \code{control_mean} and \code{treatment_mean} only).
#' @param treatment_mean A number. Average outcome in treatment. If \code{treatment_mean} is not provided then it is calculated as \code{control_mean + ate}. If both \code{ate} and  \code{treatment_mean} are provided then only  \code{treatment_mean} is used. 
#' @param verbose Logical. If TRUE, prints intra-cluster correlation implied by design parameters.
#' @return A block cluster two-arm design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment 
#' @concept blocking
#' @importFrom DeclareDesign declare_assignment declare_estimand declare_estimator declare_population declare_potential_outcomes declare_reveal
#' @importFrom fabricatr fabricate add_level
#' @importFrom randomizr conduct_ra 
#' @importFrom estimatr lm_robust
#' @importFrom rlang is_integerish
#' @importFrom stats rnorm
#' @export
#' @examples
#' # Generate a design using default arguments:
#' block_cluster_two_arm_design <- block_cluster_two_arm_designer()
#' block_cluster_uneven <- block_cluster_two_arm_designer(
#'        N_blocks = 3, N_clusters_in_block = 2:4, N_i_in_cluster = 1:9)
#' # A design in which number of clusters of cluster size is not specified
#' # but N and block size are:        
#' block_cluster_guess <- block_cluster_two_arm_designer(N = 24, N_blocks = 3)
#' # A design in which cluster size is not specified but N and block size are 
#' # and target N is not met returns an error:        
#' \dontrun{block_cluster_guess_2 <- block_cluster_two_arm_designer(N = 24,
#' N_blocks = 3, N_clusters_in_block = 3)}
#'

block_cluster_two_arm_designer <- function(N = NULL,
                                           N_blocks = 1,
                                           N_clusters_in_block = ifelse(is.null(N), 100, round(N/N_blocks)),
                                           N_i_in_cluster = ifelse(is.null(N), 1, round(N/mean(N_blocks*N_clusters_in_block))),
                                           sd = 1,
                                           sd_block = .5773*sd,
                                           sd_cluster = max(0, (sd^2 - sd_block^2)/2)^.5,
                                           sd_i_0 =     max(0,  sd^2 - sd_block^2 - sd_cluster^2)^.5,
                                           sd_i_1 = sd_i_0,
                                           rho = 1,
                                           assignment_probs = .5,
                                           control_mean = 0,
                                           ate = 0,
                                           treatment_mean = control_mean + ate,
                                           verbose = TRUE
){  
  
  if(any(N_blocks < 1, N_clusters_in_block < 1, N_i_in_cluster < 1) ||
     any(!is_integerish(N_blocks), 
         !is_integerish(N_clusters_in_block), 
         !is_integerish(N_i_in_cluster))) stop("N_* arguments must be positive integers")
  if(sd_block < 0) stop("sd_block must be nonnegative")
  if(sd_cluster < 0) stop("sd_cluster must be nonnegative")
  if(sd_i_0 < 0) stop("sd_i_0 must be nonnegative")
  if(sd_i_1 < 0) stop("sd_i_1 must be nonnegative")
  
  # Check that assignment_probss is either a scalar or of length N_blocks
  if(length(assignment_probs) != N_blocks){
    if(length(assignment_probs) == 1) assignment_probs <- rep(assignment_probs, N_blocks)
    else stop("assignment_probs must either be a scalar, or of length N_blocks.")
  }
  if(any(assignment_probs <= 0 || assignment_probs >= 1)) stop("all assignment_probs must be in (0,1)")
  
  if(rho< -1 || rho > 1) stop("correlation must be in [-1,1]")
  if(!is.null(N)) {design_N <- ifelse(length(N_i_in_cluster)>1, sum(N_i_in_cluster), sum(N_i_in_cluster*N_blocks*N_clusters_in_block))
  if(N != design_N) stop(paste0("The design N of ", design_N, " is inconsistent with the user specified N of ", N, 
                                ". Likely due to integer problems in specified block or cluster sizes or insufficient information. Better to fully specify N for each level and not provide an argument for overall N.")
    )}
  if(!is.null(N_blocks) & !is.null(N_clusters_in_block) & !is.null(N_i_in_cluster)){
    if(length(N_i_in_cluster) > 1){
      if(length(N_clusters_in_block) > 1){
        if(length(N_clusters_in_block)*N_blocks != length(N_i_in_cluster)){
          stop(paste0("You specified ",N_blocks," blocks with ",length(N_clusters_in_block)," clusters in them. Therefore N_i_in_cluster should be of length 1 or of length ",length(N_clusters_in_block)*N_blocks))
        }
      } else {
        if(N_clusters_in_block*N_blocks != length(N_i_in_cluster)){
          stop(paste0("You specified ",N_blocks," blocks with ",N_clusters_in_block," clusters in them. Therefore N_i_in_cluster should be of length 1 or of length ",N_clusters_in_block*N_blocks))
        }
      }
    }
    if(N_blocks > 1 & length(N_clusters_in_block) > 1){
      if(N_blocks!= length(N_clusters_in_block)){
        stop(paste0("You specified a design with ",N_blocks," blocks, but specified N_clusters_in_block for ",length(N_clusters_in_block)," blocks."))
      }
    }
  }
  if(verbose) print(paste0("The implied ICC in (control) is ", round(1- sd_i_0^2/(sd_i_0^2 + sd_block^2 + sd_cluster^2), 3)))
  if(verbose) print(paste0("The implied ICC in (control) conditional on block is  ", round(1- sd_i_0^2/(sd_i_0^2 + sd_cluster^2), 3)))
  if(verbose & abs(sd^2 - sd_block^2 - sd_cluster^2 - sd_i_0^2)>.0001) print(
                    paste0("Overall sd is ", 
                    round((sum(sd_block^2 + sd_cluster^2 + sd_i_0^2))^.5, 3),  
                    ", which differs from overall specified sd of ", round(sd, 3)))
    {{{    
    # M: Model
    population <- declare_population(
      blocks = add_level(
        N = N_blocks,
        u_b = rnorm(N) * sd_block),
      clusters = add_level(
        N = N_clusters_in_block,
        u_c = rnorm(N) * sd_cluster,
        cluster_size = N_i_in_cluster),
      i = add_level(
        N = N_i_in_cluster,
        u_0 = rnorm(N) * sd_i_0,
        u_1 = rnorm(n = N, mean = rho * u_0, sd = sqrt(1 - rho^2)) * sd_i_1)
    )
    
    potential_outcomes <- declare_potential_outcomes(
      Y ~ (1 - Z) * (control_mean + u_0 + u_b + u_c) + 
        Z * (treatment_mean + u_1 + u_b + u_c) )
    
    # I: Inquiry
    estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
    
    # D: Data Strategy
    assignment <- declare_assignment(block_prob = assignment_probs, blocks = blocks, clusters = clusters)
    reveal <- declare_reveal(Y, Z)
    
    # A: Answer Strategy
    estimator <- declare_estimator(
      Y ~ Z,
      estimand = estimand,
      model = lm_robust,
      fixed_effects = ~ blocks,
      clusters = clusters
    )
    
    # Design
    block_cluster_two_arm_design <- population + potential_outcomes + estimand + assignment + 
      reveal + estimator
  }}}
  
  attr(block_cluster_two_arm_design, "code") <- 
    construct_design_code(block_cluster_two_arm_designer, match.call.defaults(),
                          exclude_args = c("ate", "sd", "N"),
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
<p> A two arm blocked and clustered experiment with <code>N_blocks</code> blocks, 
each containing <code>N_clusters_in_block</code> clusters. Each cluster in turn contains 
<code>N_i_in_cluster</code> units. 
<p> Estimand is the average treatment effect (<code>ate</code>).
"



