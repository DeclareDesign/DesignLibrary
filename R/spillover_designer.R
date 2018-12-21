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
#' @param design_name A character vector. Name of design. This is the label of the design object returned by \code{get_design_code()}. Must be provided without spacing.
#' @param fixed A character vector. Names of arguments to be fixed in design. \code{design_name} is always fixed.
#' @return A simple spillover design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment
#' @concept spillovers
#' @importFrom DeclareDesign declare_assignment declare_estimand declare_estimator declare_population declare_reveal
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
                               design_name = "spillover_design",
                               fixed = NULL)
{
  if(sd_i < 0) stop("sd_i must be nonnegative")
  if(N_i_group < 1 || N_groups < 1) stop("N_i_group and N_groups must be equal to or greater than 1")
  if(grepl(" ", design_name, fixed = TRUE)) "`design_name` may not contain any spaces."
  argument_names <- names(match.call.defaults(envir = parent.frame()))[-1]
  fixed_wrong <- fixed[!fixed %in% argument_names]
  if(length(fixed_wrong)!=0) stop(paste0("The following arguments in `fixed` do not match a designer argument:", fixed_wrong)) 
  
  fixed_txt <- fixed_expr(c("N_groups", "N_i_group",  "sd_i",  "gamma"))
  for(i in 1:length(fixed_txt)) eval(parse(text = fixed_txt[i]))
  
  population_expr <- expr(declare_population(G = add_level(N = !!N_groups_, n = !!N_i_group_), 
                                             i = add_level(N = n, zeros = 0, ones = 1)))
  
  dgp_expr <- expr(function(i, Z, G, n) (sum(Z[G == G[i]])/n[i])^gamma + rnorm(1)*sd_i)
  
  {{{
    # M: Model
    population <- eval_bare(population_expr)
    
    dgp <- eval_bare(dgp_expr)
    
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
    spillover_design <- population + estimand + assignment + reveal_Y + estimator
    
  }}}
  
  design_code <- construct_design_code(spillover_designer, match.call.defaults(),
                                       arguments_as_values = TRUE,
                                       exclude_args = union(c("fixed", "design_name"), fixed))
  
  design_code <- sub_expr_text(design_code, population_expr, dgp_expr)
  
  design_code <- gsub("spillover_design <-", paste0(design_name, " <-"), design_code)
    
  attr(spillover_design, "code") <- design_code
    
  spillover_design
}

attr(spillover_designer, "shiny_arguments") <- data.frame(
  names = c("N_groups", "N_i_group", "sd_i", "gamma", "design_name", "fixed"),
  class = c("integer", "integer", "numeric", "numeric", "character", "character"),
  min = c(1, 1, 0, 0, NA, NA),
  max = c(rep(Inf, 4), NA, NA)
)
  

attr(spillover_designer, "shiny_arguments") <- list(
  N_groups = c(50, 100, 500),
  N_i_group = c(10, 50, 100),
  sd_i = c(0, .5, 1),
  gamma = c(-2, 2)
)

attr(spillover_designer, "tips") <-
  list(
    N_groups = "Number of groups",
    N_i_group = "Number of units in each group",
    sd_i = "Standard deviation of individual-level shock",
    gamma = "Parameter that controls whether spillovers within groups substitute or complement each other"
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

