#' Create a post-treatment design
#'
#' This design is an application of a simple-two-arm design where an outcome (Y) is observed conditional on a post-treatment variable (C).
#' 
#' Key limitations: Limitations here.
#' 
#' Note: Note here.
#'
#' @param N Size of sample
#' @return A post-treatment design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team} 
#' @concept post-treatment
#' @export
#' @examples
#' # To make a design using default arguments:
#' post_treatment_design <- post_treatment_designer()


post_treatment_designer <- function(N = 100
){
  {{{
    # M: Model
    population <-
      declare_population(
        N = N,
        latent_trait = rnorm(N),
        type = draw_ordered(
          latent_trait,
          breaks = c(-Inf, 0, 0.5, 2, Inf),
          break_labels = c("A", "B", "C", "D")
        )
      )
    
    potential_outcomes <-
      declare_potential_outcomes(
        C_Z_0 = as.numeric(type %in% c("A", "C")),
        C_Z_1 = as.numeric(type %in% c("A", "B")),
        Y_Z_0 = ifelse(
          type %in% c("A", "C"),
          draw_binary(latent = latent_trait, link = "probit"),
          NA
        ),
        Y_Z_1 = ifelse(
          type %in% c("A", "B"),
          draw_binary(latent = latent_trait + 0.25, link = "probit"),
          NA
        ),
        Ystar_Z_0 = ifelse(type %in% c("A", "C"), Y_Z_0, 0),
        Ystar_Z_1 = ifelse(type %in% c("A", "B"), Y_Z_1, 0)
      )
    
    # I: Inquiry
    estimand_1 <-
      declare_estimand(mean(C_Z_1 - C_Z_0), label = "ATE on C")
    estimand_2 <-
      declare_estimand(mean(Y_Z_1 - Y_Z_0), label = "ATE on Y (Conditioned by C)")
    estimand_3 <-
      declare_estimand(mean(Y_Z_1[type == "A"] - Y_Z_0[type == "A"]), label = "ATE on Y (When Y Always Observed)")
    estimand_4 <-
      declare_estimand(mean(Ystar_Z_1 - Ystar_Z_0), label = "ATE on Y (Alternative)")
    
    # D: Data Strategy
    assignment <- declare_assignment(prob = 0.5)
    
    # A: Answer Strategy
    estimator_1 <-
      declare_estimator(C ~ Z, estimand = estimand_1, label = "ATE on C")
    estimator_2 <-
      declare_estimator(Y ~ Z, estimand = c(estimand_2, estimand_3), label = "ATE on Y (Conditioned by C)")
    estimator_3 <-
      declare_estimator(Ystar ~ Z, estimand = estimand_3, label = "ATE on Y (Alternative)")
    
    # Design
    post_treatment_design <- population +
      potential_outcomes +
      assignment +
      estimand_1 +
      estimand_2 +
      estimand_3 +
      estimand_4 +
      declare_reveal(outcome_variables = c("C", "Y", "Ystar")) +
      estimator_1 +
      estimator_2 +
      estimator_3
  }}}
  
  attr(post_treatment_design, "code") <- 
    construct_design_code(post_treatment_designer, match.call.defaults())
  
  post_treatment_design
}
attr(post_treatment_designer, "tips") <- c(N = "Size of sample")
attr(post_treatment_designer, "shiny_arguments") <- list(N = c(100, 500, 1000))
attr(post_treatment_designer, "description") <- "<p> A post-treatment design with a size <code>N</code> population. This design is an application of a simple-two-arm design where an outcome (Y) is observed conditional on a post-treatment variable (C).<p>"





#' A post-treatment design
#'
#' Default design created with  \code{\link{post_treatment_designer}}
#' 
#' @seealso \code{\link{post_treatment_designer}} 
#' @seealso \code{\link{simple_two_arm_designer}} 
#' @format A design object 
"post_treatment_design"








