#' Create a design with spillovers
#'
#' Builds a design with \code{N_groups} groups each containing \code{N_i_group} individuals. 
#' Potential outcomes exhibit spillovers: if any individual in a group receives treatment, 
#' the effect is spread equally among members of the group. 
#' 
#' @details
#' 
#' Parameter \code{gamma} controls interactions between spillover effects.For \code{gamma}=1 for every $1 given to a member of a group, each member receives $1*\code{N_i_group} no matter how many others are already treated.
#' For \code{gamma}>1 (<1) for every $1 given to a member of a group, each member receives an amount that depends negatively (positively) on the number already treated.  
#' 
#' The default estimand is the average difference across subjects between no one treated and only that subject treated.  
#' 
#' @param N_groups An integer. Number of groups.
#' @param N_i_group Number of units in each group. Can be scalar or vector of length \code{N_groups}.
#' @param sd_i A nonnegative number. Standard deviation of individual-level shock.
#' @param gamma A number. Parameter that controls whether spillovers within groups substitute or complement each other. See `Details`.
#' @param args_to_fix A character vector. Names of arguments to be args_to_fix in design.
#' @return A simple spillover design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment
#' @concept spillovers
#' @importFrom DeclareDesign declare_assignment declare_inquiry declare_estimator declare_population declare_reveal
#' @importFrom fabricatr fabricate add_level fabricate
#' @importFrom randomizr conduct_ra 
#' @importFrom estimatr lm_robust
#' @aliases simple_spillover_designer
#' @export spillover_designer simple_spillover_designer
#' @examples
#' # Generate a simple spillover design using default arguments:
#' spillover_design <- spillover_designer()
#'


spillover_designer <- function(N_groups = 80, 
                               N_i_group = 3, 
                               sd_i = .2,
                               gamma = 2,
                               args_to_fix = NULL
                               )
{
  if(sd_i < 0) stop("sd_i must be nonnegative")
  if(N_i_group < 1 || N_groups < 1) stop("N_i_group and N_groups must be equal to or greater than 1")
  {{{
    # M: Model
    population <- declare_population(G = add_level(N = N_groups, n = N_i_group), 
                                     i = add_level(N = n, zeros = 0, ones = 1))
    
    dgp <- function(i, Z, G, n) (sum(Z[G == G[i]])/n[i])^gamma + rnorm(1)*sd_i
    
    # I: Inquiry
    estimand <- declare_inquiry(Treat_1 = mean(
      sapply(1:length(G), function(i) {
        Z_i <- (1:length(G)) == i
        dgp(i,Z_i,G, n) - dgp(i, zeros, G, n)})
    ), label = "estimand")
    
    # D: Data Strategy
    assignment <- declare_assignment()
    
    reveal_Y <- declare_reveal(handler=fabricate,
                               Y = sapply(1:N, function(i) dgp(i, Z, G, n)))
    
    # A: Answer Strategy
    estimator <- declare_estimator(Y ~ Z, inquiry = estimand, 
                                   model = lm_robust, label = "naive")
    
    # Design
    spillover_design <- population + estimand + assignment + reveal_Y + estimator
    
  }}}
  attr(spillover_design, "code") <- 
    construct_design_code(spillover_designer, args_to_fix = args_to_fix, match.call.defaults())
  
  spillover_design
}

attr(spillover_designer, "definitions") <- data.frame(
  names = c("N_groups", "N_i_group", "sd_i", "gamma", "args_to_fix"),
  tips  = c("Number of groups",
            "Number of units in each group",
            "Standard deviation of individual-level shock",
            "Parameter that controls whether spillovers within groups substitute or complement each other",
            "Names of arguments to be args_to_fix"),
  class = c("integer", "integer", "numeric", "numeric","character"),
  vector = c(FALSE, TRUE, FALSE, FALSE, TRUE),
  min = c(1, 1, 0, 0, NA),
  max = c(rep(Inf, 4), NA),
  inspector_min = c(100, 2, 0, 0, NA),
  inspector_step = c(50, 10, .2, .2, NA),
  stringsAsFactors = FALSE
)
  

attr(spillover_designer, "shiny_arguments") <- list(
  N_groups = c(50, 100, 500),
  N_i_group = c(10, 50, 100),
  sd_i = c(0, .5, 1),
  gamma = c(.5, 2)
)

attr(spillover_designer, "description") <- "
<p> A spillover design with <code>N_groups</code> groups each containing 
<code>N_i_group</code> individuals. Potential outcomes exhibit spillovers: if 
any individual in a group receives treatment, the effect is spread equally among 
members of the group."

simple_spillover_designer <- function(...){
  .Deprecated("spillover_designer")
  spillover_designer(...)
}

