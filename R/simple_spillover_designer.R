#' Create a simple design with spillovers
#'
#' This designer builds a design with \code{n_groups} groups each containing \code{group_size} individuals. 
#' Potential outcomes exhibit spillovers: if any individual in a group receives treatment, 
#' the effect is spread equally among members of the group. 
#' Parameter \code{gamma} controls interactions between spillover effects. 
#' For \code{gamma}=1 for ever $1 given to a member of a group, each member receives $1\code{group_size} no matter how many others are already treated.
#' For \code{gamma}>1 (<1) for ever $1 given to a member of a group, each member receives an amount that depends negatively (positively) on the number already treated.  
#' The default estimand is the average difference 
#' across subjects between no one treated and only that subject treated.  
#' 
#' 
#' @param code Logical. If TRUE, returns the code of a design, otherwise returns a design.
#' @param N_groups Number of groups.
#' @param group_size Number of units in each group. May be scalar, or vector of length(n_groups) 
#' @param sd Standard deviation of individual level shock
#' @param gamma Parameter that controls whether spillovers within groups substitue for each other or complement each other
#' @return A function that returns a design.
#' @export
#'
#' @examples
#' # To make a design using default arguments:
#' simple_spillover_design <- simple_spillover_designer()
#'


simple_spillover_designer <- function(n_groups = 80, group_size = 3, sd = .2, gamma = 2, code = FALSE)
{

  design_code <- function() {
    # Below is grabbed by get_design_code
    
    {{{

# Model ----------------------------------------------------------------------

pop <- declare_population(G = add_level(N = n_groups, n = group_size), 
                          i = add_level(N = n, zeros = 0, ones =1))
      
dgp <- function(i, Z, G, n) (sum(Z[G == G[i]])/n[i])^gamma + rnorm(1)*sd
      
# Inquiry --------------------------------------------------------------------
estimand <- declare_estimand(Treat_1 = mean(
        sapply(1:length(G), function(i) {
          Z_i <- (1:length(G))==i
          dgp(i,Z_i,G, n) - dgp(i, zeros, G, n)})
      ), label = "estimand")
      # Data
      assignt <- declare_assignment()
      
reveal <- declare_reveal(handler=fabricate,
                         Y = sapply(1:N, function(i) dgp(i, Z, G, n)))
      
# Answer Strategy -------------------------------------------------------------
estimator <- declare_estimator(Y ~ Z, estimand = "Treat_1", 
                               model = lm_robust, label = naive)

# Design ----------------------------------------------------------------------
simple_spillover_design <- pop / estimand / assignt / reveal /  estimator
      
    }}}
    simple_spillover_design
  }
  
  if (code)
    out <- get_design_code(design_code)
  else
    out <- design_code()
  return(out)
}

