#' Create an audit experiment design
#'
#' Description here
#' 
#' Key limitations: Limitations here.
#' 
#' Note: Note here.
#'
#' @param N Size of sample
#' @return An audit experiment design.
#' @author DeclareDesign Team \url{https://declaredesign.org/}
#' @concept experiment
#' @export
#' @examples
#' # To make a design using default arguments:
#' audit_experiment_design <- audit_experiment_designer()


audit_experiment_designer <- function(N = 100
){
  {{{
    # M: Model
    population <-
      declare_population(
        N = N,
        discrimination = rnorm(N),
        type = draw_ordered(
          discrimination,
          breaks = c(-Inf, 0, 0.5, 2, Inf),
          break_labels = c("A", "B", "C", "D")
        )
      )
    
    potential_outcomes <-
      declare_potential_outcomes(
        R_Z_0 = as.numeric(type %in% c("A", "C")),
        R_Z_1 = as.numeric(type %in% c("A", "B")),
        Y_Z_0 = ifelse(
          type %in% c("A", "C"),
          draw_binary(latent = discrimination, link = "probit"),
          NA
        ),
        Y_Z_1 = ifelse(
          type %in% c("A", "B"),
          draw_binary(latent = discrimination + 0.25, link = "probit"),
          NA
        ),
        Ystar_Z_0 = ifelse(type %in% c("A", "C"), Y_Z_0, 0),
        Ystar_Z_1 = ifelse(type %in% c("A", "B"), Y_Z_1, 0)
      )
    
    # I: Inquiry
    estimand_1 <-
      declare_estimand(mean(R_Z_1 - R_Z_0), label = "ATE on Response")
    estimand_2 <-
      declare_estimand(mean(Y_Z_1[type == "A"] - Y_Z_0[type == "A"]), label = "ATE on Tone")
    estimand_3 <-
      declare_estimand(mean(Ystar_Z_1 - Ystar_Z_0), label = "ATE on Tone (Alternative)")
    
    # D: Data Strategy
    assignment <- declare_assignment(prob = 0.5)
    
    # A: Answer Strategy
    estimator_1 <-
      declare_estimator(R ~ Z, estimand = estimand_1, label = "ATE on Response")
    estimator_2 <-
      declare_estimator(Y ~ Z, estimand = estimand_2, label = "ATE on Tone")
    estimator_3 <-
      declare_estimator(Ystar ~ Z, estimand = estimand_3, label = "ATE on Tone (Alternative)")
    
    # Design
    audit_experiment_design <- declare_design(
      population,
      potential_outcomes,
      assignment,
      estimand_1,
      estimand_2,
      estimand_3,
      declare_reveal(outcome_variables = c("R", "Y", "Ystar")),
      estimator_1,
      estimator_2,
      estimator_3)
  }}}
  
  attr(audit_experiment_design, "code") <- 
    construct_design_code(audit_experiment_designer, match.call.defaults())
  
  audit_experiment_design
}
attr(audit_experiment_designer, "tips") <- c(N = "Size of sample")
attr(audit_experiment_designer, "shiny_arguments") <- list(N = c(100, 500, 1000))
attr(audit_experiment_designer, "description") <- "<p> An audit experiment with a size <code>N</code> population <p>"





#' An audit experiment design
#'
#' Default design created with  \code{\link{audit_experiment_designer}}
#' 
#' @seealso \code{\link{audit_experiment_designer}} 
#' @format A design object 
"audit_experiment_design"








