#' Create a block and cluster 2 by 2 factorial design with binary outcomes option
#'
#' Builds a two-by-two factorial design in which assignments to each factor are independent of each other, allows blocks an clusters.
#' 
#' @details
#' Three types of estimand are declared. First, weighted averages of the average treatment effects of each treatment, given the two conditions of the other treatments. Second and third, the difference in treatment effects of each treatment, given the conditions of the other treatment.
#' 
#' Units are assigned to treatment using complete random assignment. Potential outcomes follow a normal distribution.
#' 
#' Treatment A is assigned first and then Treatment B within blocks defined by treatment A. Thus, if there are 6 units 
#' 3 are guaranteed to receive treatment A but the number receiving treatment B is stochastic.
#' 
#' See \code{\link{multi_arm_designer}} for a factorial design with non independent assignments.
#' 
#' @param N An integer. Total number of units. Usually not specified as \code{N} is determined by \code{N_blocks}, \code{N_clusters_in_block}, and \code{N_i_in_cluster}. If \code{N_blocks}, and  \code{N_clusters_in_block}, and \code{N_i_in_cluster} are specified then \code{N} is overridden. If these are not specified and \code{N} is specified then designer attempts to guess sizes of levels to approximate \code{N}, with preference for a design without blocks or clusters. 
#' @param N_blocks An integer. Number of blocks. Defaults to 1 for no blocks. 
#' @param N_clusters_in_block An integer or vector of integers of length \code{N_blocks}. Number of clusters in each block. This is the total \code{N} when \code{N_blocks} and \code{N_i_in_cluster} are at default values. 
#' @param N_i_in_cluster An integer or vector of integers of length \code{sum(N_clusters_in_block)}. Individuals per cluster. Defaults to 1 for no clusters.
#' @param binary Logical: a binary outcome generated using probit approach.
#' @param prob_A A number in [0,1]. Probability of assignment to treatment A.
#' @param prob_B A number in [0,1]. Probability of assignment to treatment B.
#' @param weight_A A number. Weight placed on A=1 condition in definition of "average effect of B" estimand.
#' @param weight_B A number. Weight placed on B=1 condition in definition of "average effect of A" estimand.
#' @param outcome_means A vector of length 4. Average outcome in each A,B condition, in order AB = 00, 01, 10, 11. Values overridden by mean_A0B0, mean_A0B1, mean_A1B0, if provided mean_A1B1.
#' @param mean_A0B0 A number. Mean outcome in A=0, B=0 condition.
#' @param mean_A0B1 A number. Mean outcome in A=0, B=1 condition.
#' @param mean_A1B0 A number. Mean outcome in A=1, B=0 condition.
#' @param mean_A1B1 A number. Mean outcome in A=1, B=1 condition.
#' @param sd A nonnegative number. Overall standard deviation (combining individual level, cluster level, and block level shocks). Defaults to 1. Overridden if incompatible with other user-specified shocks. 
#' @param sd_block A nonnegative number. Standard deviation of block level shocks.
#' @param sd_cluster A nonnegative number. Standard deviation of cluster level shock.
#' @param sd_i A nonnegative number. Standard deviation of individual level shock.
#' @param verbose Logical. Return details on design.
#' @return A two-by-two factorial design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment factorial
#' @importFrom DeclareDesign declare_assignment declare_estimand declare_estimator declare_population declare_potential_outcomes declare_reveal diagnose_design redesign
#' @importFrom fabricatr fabricate 
#' @importFrom randomizr conduct_ra 
#' @importFrom estimatr tidy lm_robust
#' @export
#'
#' @examples
#' design <- block_cluster_2x2_factorial_designer(outcome_means = c(0,0,0,1))
#' 
#' # A design biased for the specified estimands:
#' design <- block_cluster_2x2_factorial_designer(outcome_means = c(0,0,0,1), prob_A = .8, prob_B = .2)
#' \dontrun{
#' diagnose_design(design)
#' }
#' 
#' # A design with estimands that "match" the assignment:
#' design <- block_cluster_2x2_factorial_designer(outcome_means = c(0,0,0,1), 
#'                                     prob_A = .8, prob_B = .2, 
#'                                     weight_A = .8, weight_B = .2)
#' \dontrun{
#' diagnose_design(design)
#' }
#' 
#' # Compare power with and without interactions, given same average effects in each arm
#' designs <- redesign(block_cluster_2x2_factorial_designer(), 
#'                     outcome_means = list(c(0,0,0,1), c(0,.5,.5,1)))
#' \dontrun{
#' diagnose_design(designs)
#' }
#' 
block_cluster_2x2_factorial_designer <- function(
  N = NULL,
  N_blocks = 1,
  N_clusters_in_block = ifelse(is.null(N), 100, round(N / N_blocks)),
  N_i_in_cluster = ifelse(is.null(N), 2, round(N /
    mean(N_blocks * N_clusters_in_block))),
  binary = FALSE,
  prob_A = .5,
  prob_B = .5,
  weight_A = .5,
  weight_B = .5,
  outcome_means = rep(0, 4),
  mean_A0B0 = outcome_means[1],
  mean_A0B1 = outcome_means[2],
  mean_A1B0 = outcome_means[3],
  mean_A1B1 = outcome_means[4],
  sd = 1,
  sd_block   = .5773 * sd,
  sd_cluster = max(0, (sd ^ 2 - sd_block ^ 2) / 2) ^ .5,
  sd_i       =     max(0,  sd ^ 2 - sd_block ^ 2 - sd_cluster ^ 2) ^ .5,
  verbose = TRUE) {
  
    if ((weight_A < 0) ||
        (weight_B < 0) ||
        (weight_A > 1) ||
        (weight_B > 1))
      stop("weight_A and weight_B must be in [0,1]")
    if(max(c(prob_A, prob_B) < 0)) stop("prob_ arguments must be nonnegative")
    if(max(c(prob_A, prob_B) > 1))  stop("prob_ arguments must not exceed 1")
  
    if(any(N_blocks < 1, N_clusters_in_block < 1, N_i_in_cluster < 1) ||
       any(!is_integerish(N_blocks), 
           !is_integerish(N_clusters_in_block), 
           !is_integerish(N_i_in_cluster))) stop("N_* arguments must be positive integers")
    if(sd_block < 0) stop("sd_block must be nonnegative")
    if(sd_cluster < 0) stop("sd_cluster must be nonnegative")
    if(sd_i < 0) stop("sd_i must be nonnegative")
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
    if(verbose & !binary) print(paste0("The implied ICC on linear outcome is ", round(1- sd_i^2/(sd_i^2 + sd_block^2 + sd_cluster^2), 3)))
    if(verbose & !binary) print(paste0("The implied ICC on linear outcome conditional on block is  ", round(1- sd_i^2/(sd_i^2 + sd_cluster^2), 3)))
    if(verbose & abs(sd^2 - sd_block^2 - sd_cluster^2 - sd_i^2)>.0001) print(
      paste0("Overall sd (on non transformed outcome) is ", 
             round((sum(sd_block^2 + sd_cluster^2 + sd_i^2))^.5, 3),  
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
          u_i = rnorm(N) * sd_i)
      )
      
      potential_outcomes <- declare_potential_outcomes(
        Y ~ (1 - Z) * (control_mean + u_i + u_b + u_c) + 
          Z * (treatment_mean + u_1 + u_b + u_c) )
      
      transform <- ifelse(binary, function(u, v) 1*(u < v), function(u, v) u + v)
      
      potential_outcomes <- declare_potential_outcomes(
        Y_A_0_B_0 = transform(u_i, mean_A0B0 + u_b + u_c),  
        Y_A_0_B_1 = transform(u_i, mean_A0B1 + u_b + u_c),  
        Y_A_1_B_0 = transform(u_i, mean_A1B0 + u_b + u_c),
        Y_A_1_B_1 = transform(u_i, mean_A1B1 + u_b + u_c))
      
      
      # I: Inquiry
      estimands <- declare_estimand(
        control_mean = mean(Y_A_0_B_0),
        ate_A = weight_B*mean(Y_A_1_B_1 - Y_A_0_B_1) + (1-weight_B)*mean(Y_A_1_B_0 - Y_A_0_B_0),
        ate_B = weight_A*mean(Y_A_1_B_1 - Y_A_1_B_0) + (1-weight_A)*mean(Y_A_0_B_1 - Y_A_0_B_0),
        interaction = mean((Y_A_1_B_1 - Y_A_1_B_0) - (Y_A_0_B_1 - Y_A_0_B_0)))
      
      # D: Data Strategy
      
      # Factorial assignments
      assign_A <- declare_assignment(prob = prob_A, assignment_variable = A, blocks = blocks, clusters = clusters)
      assign_B <- declare_assignment(prob = prob_B, assignment_variable = B, blocks = paste(A, blocks, sep = "_"), 
                                     clusters = clusters)
      reveal_Y <- declare_reveal(Y_variables = Y, assignment_variables = c(A,B))
      
      # A: Answer Strategy
      estimator_0 <- declare_estimator(Y ~ A + B,
                                       model = lm_robust,
                                       term = c("(Intercept)"),
                                       clusters = clusters,
                                       estimand = c("control_mean"), 
                                       label = "Control Mean")
      estimator_1 <- declare_estimator(Y ~ A + B,
                                       model = lm_robust,
                                       term = c("A", "B"),
                                       fixed_effects = ~ blocks,
                                       clusters = clusters,
                                       estimand = c("ate_A", "ate_B"), 
                                       label = "No_Interaction")
      estimator_2 <- declare_estimator(Y ~ A + B + A:B,
                                       model = lm_robust,
                                       term = "A:B", 
                                       fixed_effects = ~ blocks,
                                       clusters = clusters,
                                       estimand = "interaction", 
                                       label = "Interaction")
      
      ICC_function <- function(data){
        e   <- with(data, Y - ave(Y, blocks,   FUN = function(x) mean(x, na.rm = TRUE)))    
        e2  <- with(data, e - ave(e, clusters, FUN = function(x) mean(x, na.rm = TRUE)))
        adj <- (nrow(data)-1)/(nrow(data) - length(unique(data$clusters)) - 1)
        
        data.frame(
          estimator_label = "ICC",
          estimand_label = "None",
          estimate = 1 - adj*var(e2, na.rm = TRUE)/var(e, na.rm = TRUE),
          term = "ICC",
          stringsAsFactors = FALSE
        )
      }


      
      get_ICC <- declare_estimator(handler = ICC_function)
      
      # Design
      block_cluster_2x2_factorial_design <- population + potential_outcomes + 
        estimands +
        assign_A + assign_B + reveal_Y + 
        estimator_0 + estimator_1 + estimator_2 + get_ICC
    
  }}}
  
  attr(block_cluster_2x2_factorial_design, "code") <- 
    construct_design_code(designer = block_cluster_2x2_factorial_designer, 
                          args = match.call.defaults(), 
                          exclude_args = c("N", "outcome_means"),
                          arguments_as_values = TRUE)
  
  block_cluster_2x2_factorial_design
}


attr(block_cluster_2x2_factorial_designer, "shiny_arguments") <- list(
  N = c(16, 32, 64), weight_A = c(0, .5), 
  mean_A0B1 = 0:1, 
  mean_A1B0 = 0:1, 
  mean_A1B1 = -1:3) 

attr(block_cluster_2x2_factorial_designer, "tips") <-
  list(
    N = "Sample size",
    weight_A = "Weight on B=1 condition for effect of A estimand",
    mean_A1B0 = "Mean outcome for A=1, B=0",
    mean_A0B1 = "Mean outcome for A=0, B=1",
    mean_A1B1 = "Mean outcome for A=1, B=1"
  )

attr(block_cluster_2x2_factorial_designer, "description") <- "
<p> A 2x2 factorial design of sample size <code>N</code> with blocks and clusters and independent treatment assignment.
"
