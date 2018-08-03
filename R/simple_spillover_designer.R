#' Create a simple design with spillovers
#'
#' Builds a design with \code{N_groups} groups each containing \code{N_i_group} individuals. 
#' Potential outcomes exhibit spillovers: if any individual in a group receives treatment, 
#' the effect is spread equally among members of the group. 
#' 
#' @details
#' 
#' Parameter \code{gamma} controls interactions between spillover effects.For \code{gamma}=1 for ever $1 given to a member of a group, each member receives $1\code{N_i_group} no matter how many others are already treated.
#' For \code{gamma}>1 (<1) for ever $1 given to a member of a group, each member receives an amount that depends negatively (positively) on the number already treated.  
#' 
#' The default estimand is the average difference across subjects between no one treated and only that subject treated.  
#' 
#' @param N_groups An integer. Number of groups.
#' @param N_i_group Number of units in each group. Can be scalar or vector of length \code{N_groups}.
#' @param sd A number. Standard deviation of individual level shock.
#' @param gamma A number. Parameter that controls whether spillovers within groups substitute or complement each other.
#' @return A simple spillover design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment
#' @concept spillovers
#' @import DeclareDesign stats utils fabricatr estimatr randomizr
#' @export
#' @examples
#' # Generate a simple spillover design using default arguments:
#' simple_spillover_design <- simple_spillover_designer()
#'


simple_spillover_designer <- function(N_groups = 80, 
                                      N_i_group = 3, 
                                      sd = .2,
                                      gamma = 2)
{
  N <- n <- G <- zeros <- Z <- NULL
  if(sd < 0) stop("sd must be non-negative")
  if(N_i_group < 1 || N_groups < 1) stop("N_i_group and N_groups must be greater than 1")
  {{{
    # M: Model
    population <- declare_population(G = add_level(N = N_groups, n = N_i_group), 
                              i = add_level(N = n, zeros = 0, ones = 1))
    
    dgp <- function(i, Z, G, n) (sum(Z[G == G[i]])/n[i])^gamma + rnorm(1)*sd
    
    # I: Inquiry
    estimand <- declare_estimand(Treat_1 = mean(
      sapply(1:length(G), function(i) {
        Z_i <- (1:length(G)) == i
        dgp(i,Z_i,G, n) - dgp(i, zeros, G, n)})
    ), label = "estimand")
    
    # D: Data Strategy
    assignment <- declare_assignment()
    
    reveal_Y <- declare_reveal(handler=fabricate,
                             Y = sapply(1:N, function(i) dgp(i, Z, G, n)))
    
    # A: Answer Strategy
    estimator <- declare_estimator(Y ~ Z, estimand = estimand, 
                                   model = lm_robust, label = "naive")
    
    # Design
    simple_spillover_design <- population + estimand + assignment + reveal_Y + estimator
    
  }}}
  attr(simple_spillover_design, "code") <- 
    construct_design_code(simple_spillover_designer, match.call.defaults())
  
  simple_spillover_design
}

