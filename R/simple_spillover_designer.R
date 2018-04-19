#' Create a simple design with spillovers
#'
#' This designer builds a design with \code{n_groups} groups each containing \code{group_size} individuals. 
#' Potential outcomes exhibit spillovers: if any individual in a group receives treatment, 
#' the effect is equally spread among members of the group. 
#' Intuitively, for ever $1 given to a member of a group, each member receives $1\code{group_size}
#' The default estimand is the average difference 
#' across subjects between no one treated and only that subject treated.  
#' 
#' 
#' @param code Logical. If TRUE, returns the code of a design, otherwise returns a design.
#' @param N_groups Number of groups.
#' @param group_size Number of units in each group
#' @param sd Standard deviation of individual level shock
#' @return A function that returns a design.
#' @export
#'
#' @examples
#' # To make a design using default arguments:
#' simple_spillover_design <- simple_spillover_designer()
#'


simple_spillover_designer <- function(n_groups = 40, group_size = 3, sd =1, code = FALSE)
{

  design_code <- function() {
    # Below is grabbed by get_design_code
    
    {{{

      # Model ----------------------------------------------------------------------
      pop <- declare_population(N = n_groups*group_size, 
                                G = rep(1:n_groups, each = group_size))
      dgp <- function(i, Z, G) sum(Z[G == G[i]])/group_size + rnorm(1)*sd
      
      # Inquiry --------------------------------------------------------------------
      estimand <- declare_estimand(ATE = mean(
        sapply(1:length(G), function(i) {
          Z0 = rep(0, length(G))
          Z1 = (1:length(G))==i
          dgp(i,Z1,G) - dgp(i, Z0, G)})
      ))
      # Data
      assignt <- declare_assignment()
      
      reveal <- declare_reveal(handler=fabricate,
                               Y = sapply(1:N, function(i) dgp(i, Z, G)))
      
      # Answer Strategy -------------------------------------------------------------
      estimator <- declare_estimator(Y ~ Z, estimand = estimand, model = lm_robust)
      
      # Design ----------------------------------------------------------------------
      simple_spillover_design <- pop / estimand / assignt / reveal/  estimator
      
    }}}
    simple_spillover_design
  }
  
  if (code)
    out <- get_design_code(design_code)
  else
    out <- design_code()
  return(out)
}

