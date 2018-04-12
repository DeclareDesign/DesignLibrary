#' Create a simple two arm design
#' 
#' Note: Default arguments produce a design without blocks and clusters and with N determined by `N_cluster_in_block`
#' 
#' @param code If TRUE template returns the code of a design 
#' @param N_blocks Number of blocks
#' @param N_clusters_in_block Number of clusters per block
#' @param N_i_in_cluster Individuals per block
#' @param sd_block Standard deviation of block level shocks
#' @param sd_cluster Standard deviation of cluster level shock
#' @param sd_i Standard deviation of individual level shock 
#' @param prob Assignment probability
#' @param control_mean Average outcome in control
#' @param ate  Average treatment effect 
#' @param treatment_mean Average outcome in treatment
#' @return a function that returns a design
#' @export
#'
#' @examples
#' block_cluster_two_arm_design <- block_cluster_two_arm_designer()
#' block_cluster_two_arm_designer(code = TRUE)


block_cluster_two_arm_designer <- function(
                                    N_blocks = 1,
                                    N_clusters_in_block = 100,
                                    N_i_in_cluster = 1,
                                    sd_block = .2,
                                    sd_cluster = .2,
                                    sd_i = max(0, 1 - sd_block - sd_cluster),
                                    prob = .5,
                                    control_mean = 0,
                                    ate = 1,
                                    treatment_mean = control_mean + ate,
                                    code = FALSE
){
  
  # Below is grabbed by get_design_code

    
design_code <- function() { 
  
  {{{
  
  # M: Model
   pop <- declare_population(
      blocks   = add_level(N = N_blocks, 
                           u_b = rnorm(N)*sd_block),
      clusters = add_level(N = N_clusters_in_block, 
                           u_c = rnorm(N)*sd_cluster,
                           cluster_size = N_i_in_cluster),
      i        = add_level(N  = N_i_in_cluster, 
                           u_i = rnorm(N)*sd_i,
                           Z0 = u_i + u_b + u_c,
                           Z1 = Z0 + treatment_mean)
      )
    
   potential_outcomes <- declare_potential_outcomes(Y ~ (1-Z)*Z0 + Z * Z1)
    
    # I: Inquiry
    estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
    
    # D: Data
    assignment <- declare_assignment(prob = prob, blocks = blocks, clusters = clusters)
    
    # A: Analysis
    estimator <- declare_estimator(Y ~ Z, estimand = estimand, 
                                   model = difference_in_means, 
                                   blocks = blocks, 
                                   clusters = clusters)
    
    # Design
    block_cluster_two_arm <- pop / potential_outcomes / estimand /assignment / declare_reveal() /estimator
    
  }}}    

  block_cluster_two_arm
  }

if(code)  out <- get_design_code(design_code)
if(!code) out <- design_code()
return(out)
}

attr(block_cluster_two_arm_designer, "shiny_args") <- list(N_blocks = c(10, 20, 50), N_cluster_in_block = c(2, 4),
                                                           N_i_in_cluster = c(1, 5, 10), ate = c(0, .1, .3)) 

