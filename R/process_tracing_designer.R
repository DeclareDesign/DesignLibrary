#' Create a process-tracing design
#'
#' Builds a design in which two pieces of evidence are sought and used to update about whether X caused Y using Bayes' rule.
#' 
#' @details
#' 
#' The model posits a population of \code{N} cases, each of which does or does not exhibit the presence of some outcome, Y. With probability \code{prob_X}, each case also exhibits the presence or absence of some potential cause, X. The outcome Y can be realized through four distinct causal relations, distributed through the population of cases according to \code{process_proportions}. First, the presence of X might cause Y. Second, the absence of X might cause Y. Third, Y might be present irrespective of X. Fourth, Y might be absent irrespective of X.
#' 
#' Our inquiry is a "cause of effects" question. We wish to know whether a specific case was one in which the presence (absence) of X caused the presence (absence) of Y. 
#' 
#' Our data strategy consists of selecting one case at random in which both X and Y are present. As part of the data strategy we seek two pieces of evidence in favor of or against the hypothesized causal relationship, H, in which X causes Y. 
#' 
#' The first (second) piece of evidence is observed with probability \code{p_E1_H} (\code{p_E2_H}) when H is true, and with probability \code{p_E1_not_H} (\code{p_E2_not_H}) when H is false.
#' 
#' Conditional on H being true (false), the correlation between the two pieces of evidence is given by \code{cor_E1E2_H} (\code{cor_E1E2_not_H}).  
#' 
#' The researcher uses Bayes’ rule to update about the probability that X caused Y given the evidence. In other words, they form a posterior inference, Pr(H|E). We specify four answer strategies for forming this inference. The first simply ignores the evidence and is equivalent to stating a prior belief without doing any causal process tracing. The second conditions inferences only on the first piece of evidence, and the third only on the second piece of evidence. The fourth strategy conditions posterior inferences on both pieces of evidence simultaneously.
#' 
#' We specify as diagnosands for this design the bias, RMSE, mean(estimand), mean(estimate) and sd(estimate).
#' 
#' @param N An integer. Size of population of cases from which a single case is selected.
#' @param prior_H A number in [0,1]. Prior probability that X causes Y in a given case in which X and Y are both present. 
#' @param prob_X A number in [0,1]. Probability that X = 1 for a given case (equal throughout population of cases).
#' @param process_proportions A vector of numbers in [0,1] that sums to 1. Simplex denoting the proportion of cases in the population in which, respectively: 1) X causes Y; 2) Y occurs regardless of X; 3) X causes the absence of Y; 4) Y is absent regardless of X.
#' @param p_E1_H A number in [0,1]. Probability of observing first piece of evidence given hypothesis that X caused Y is true. 
#' @param p_E2_H A number in [0,1]. Probability of observing second piece of evidence given hypothesis that X caused Y is true. 
#' @param p_E1_not_H A number in [0,1]. Probability of observing first piece of evidence given hypothesis that X caused Y is not true. 
#' @param p_E2_not_H A number in [0,1]. Probability of observing second piece of evidence given hypothesis that X caused Y is not true. 
#' @param cor_E1E2_H A number in [-1,1]. Correlation between first and second pieces of evidence given hypothesis that X caused Y is true. 
#' @param cor_E1E2_not_H A number in [-1,1]. Correlation between first and second pieces of evidence given hypothesis that X caused Y is not true. 
#' @param label_E1 A string. Label for the first piece of evidence (e.g., "Smoking Gun").
#' @param label_E2 A string. Label for the second piece of evidence (e.g., "Straw in the Wind").
#' @return A process-tracing design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept qualitative 
#' @concept process tracing
#' @importFrom DeclareDesign declare_diagnosands declare_estimand declare_estimator declare_population declare_sampling declare_step diagnose_design draw_data get_estimands get_estimates set_diagnosands
#' @importFrom fabricatr fabricate fabricate
#' @importFrom randomizr conduct_ra draw_rs 
#' @importFrom estimatr tidy
#' @importFrom stats rbinom
#' @importFrom rlang is_integerish is_character
#' @importFrom utils data
#' @export
#' @examples
#' # Generate a process-tracing design using default arguments:
#' pt_1 <- process_tracing_designer()
#' get_estimands(pt_1)
#' get_estimates(pt_1)
#' draw_data(pt_1)
#' \dontrun{
#' diagnose_design(pt_1, sims = 1000)
#' }
#' 
#' # A design in which the smoking gun and straw-in-the-wind are correlated
#' pt_2 <- process_tracing_designer(cor_E1E2_H = .32)
#' \dontrun{
#' diagnose_design(pt_2, sims = 1000)
#' }
#' 
#' # A design with two doubly-decisive tests pointing in opposite directions
#' pt_3 <- process_tracing_designer(p_E1_H = .80,p_E1_not_H = .05,
#'                                  label_E1 = "Doubly-Decisive: H",
#'                                  p_E2_H = .05,p_E2_not_H = .80,
#'                                  label_E2 = "Doubly-Decisive: Not H")
#' get_estimates(pt_3)                                
#' \dontrun{
#' diagnose_design(pt_3, sims = 1000)
#' }
#'
process_tracing_designer <- function(
  N = 100,
  prob_X = .5,
  process_proportions = c('X_causes_Y' = .25, 'Y_regardless' = .25,
                          'X_causes_not_Y' = .25, 'not_Y_regardless' = .25),
  prior_H = .5,
  p_E1_H = .3,
  p_E1_not_H = 0,
  p_E2_H = .8,
  p_E2_not_H = .2,
  cor_E1E2_H = 0,
  cor_E1E2_not_H = 0,
  label_E1 = "Smoking Gun",
  label_E2 = "Straw in the Wind"
){
  if(!is_integerish(N) || N < 1) stop("N must be a positive integer.")
  if(prob_X < 0 || prob_X > 1) stop("prob_X must be in [0,1].")
  if(length(process_proportions) != 4) stop("process_proportions must be of length 4.")
  if(sum(process_proportions) != 1 || any(process_proportions < 0) || any(process_proportions> 1)) stop("process_proportions must be a 3-simplex.")
  if(prior_H < 0 || prior_H > 1) stop("prior_H must be in [0,1].")
  if(p_E1_H < 0 || p_E1_H > 1) stop("p_E1_H must be in [0,1].")
  if(p_E2_H < 0 || p_E2_H > 1) stop("p_E2_H must be in [0,1].")
  if(p_E1_not_H < 0 || p_E1_not_H > 1) stop("p_E1_not_H must be in [0,1].")
  if(p_E2_not_H < 0 || p_E2_not_H > 1) stop("p_E2_not_H must be in [0,1].")
  if(abs(cor_E1E2_H) > 1) stop("cor_E1E2_H must be in [-1,1].")
  if(abs(cor_E1E2_not_H) > 1) stop("cor_E1E2_not_H must be in [-1,1].")
  test_prob <- function(p1, p2, rho) {
    r = rho * (p1 * p2 * (1 - p1) * (1 - p2))^.5
    c((1 - p1) * (1 - p2) + r,p2 * (1 - p1) - r, p1 * (1 - p2) - r, p1 * p2 + r)}
  if(min(test_prob(p_E1_H, p_E2_H, cor_E1E2_H)) < 0) stop("Correlation coefficient not compatible with probabilities")
  if(min(test_prob(p_E1_not_H, p_E2_not_H, cor_E1E2_not_H)) < 0) stop("Correlation coefficient not compatible with probabilities")
  if(!is_character(label_E1) || length(label_E1) > 1) stop("label_E1 must be a character of length 1.")
  if(!is_character(label_E2) || length(label_E2) > 1) stop("label_E2 must be a character of length 1.")
  {{{
    # M: Model
    population <- declare_population(
      N = N,
      causal_process = sample(
        x = c('X_causes_Y', 'Y_regardless',
              'X_causes_not_Y', 'not_Y_regardless'),
        size = N,
        replace = TRUE,
        prob = process_proportions),
      X = rbinom(N, 1, prob_X) == 1,
      Y = (X & causal_process == "X_causes_Y") | # 1. X causes Y
        (!X & causal_process == "X_causes_not_Y") | # 2. Not X causes Y
        (causal_process == "Y_regardless") # 3. Y happens irrespective of X
    )
    # D: Data Strategy 1
    select_case <- declare_sampling(
      strata = paste(X, Y),
      strata_n = c("X0Y0" = 0, "X0Y1" = 0, "X1Y0" = 0, "X1Y1" = 1))
    # I: Inquiry
    estimand <-
      declare_estimand(did_X_cause_Y = causal_process == 'X_causes_Y')
    # D: Data Strategy 2
    # Calculate bivariate probabilities given correlation
    joint_prob <- function(p1, p2, rho) {
      r <- rho * (p1 * p2 * (1 - p1) * (1 - p2)) ^ .5
      c(
        p00 = (1 - p1) * (1 - p2) + r,
        p01 = p2 * (1 - p1) - r,
        p10 = p1 * (1 - p2) - r,
        p11 = p1 * p2 + r)}
    joint_prob_H <- joint_prob(p_E1_H, p_E2_H, cor_E1E2_H)
    joint_prob_not_H <- joint_prob(p_E1_not_H, p_E2_not_H, cor_E1E2_not_H)
    
    trace_processes <- declare_step(
      test_results = sample(
        c("00", "01", "10", "11"),1, 
        prob = ifelse(rep(causal_process == "X_causes_Y", 4),
                      joint_prob_H,
                      joint_prob_not_H)),
      E1 = test_results == "10" | test_results == "11",
      E2 = test_results == "01" | test_results == "11",
      handler = fabricate)
    
    # A: Answer Strategy
    bayes_rule <- function(p_H, p_E_H, p_E_not_H) {
      p_E_H * p_H / (p_E_H * p_H + p_E_not_H * (1 - p_H))}
    
    prior_only <- function(data){
      return(with(data,
                  data.frame(
                    posterior_H = bayes_rule(p_H = prior_H, p_E_H = 1, p_E_not_H = 1),
                    result = TRUE)
      ))}
    E1_only <- function(data){
      return(with(data,
                  data.frame(
                    posterior_H = bayes_rule(
                      p_H = prior_H, 
                      p_E_H = ifelse(E1, p_E1_H, 1 - p_E1_H), 
                      p_E_not_H = ifelse(E1, p_E1_not_H, 1 - p_E1_not_H)),
                    result = E1))
      )}
    E2_only <- function(data){
      return(with(data,
                  data.frame(
                    posterior_H = bayes_rule(
                      p_H = prior_H, 
                      p_E_H = ifelse(E2, p_E2_H, 1 - p_E2_H), 
                      p_E_not_H = ifelse(E2, p_E2_not_H, 1 - p_E2_not_H)),
                    result = E2))
      )}
    E1_and_E2 <- function(data){
      return(with(data,
                  data.frame(
                    posterior_H = bayes_rule(
                      p_H = prior_H, 
                      p_E_H = joint_prob_H[c("00", "01", "10", "11") %in% test_results],
                      p_E_not_H = joint_prob_not_H[c("00", "01", "10", "11") %in% test_results]),
                    result = test_results)
      ))}
    
    prior_only_estimator <- declare_estimator(
      handler = tidy_estimator(prior_only),
      label = "No tests (Prior)",
      estimand = estimand
    )
    E1_only_estimator <- declare_estimator(
      handler = tidy_estimator(E1_only),
      label = label_E1,
      estimand = estimand
    )
    E2_only_estimator <- declare_estimator(
      handler = tidy_estimator(E2_only),
      label = label_E2,
      estimand = estimand
    )
    E1_and_E2_estimator <- declare_estimator(
      handler = tidy_estimator(E1_and_E2),
      label = paste(label_E1, "and", label_E2),
      estimand = estimand
    )
    
    # Design
    process_tracing_design <-
      population + select_case + trace_processes + estimand +
      prior_only_estimator + E1_only_estimator + E2_only_estimator + 
      E1_and_E2_estimator
    
    
  }}}
  
  attr(process_tracing_design, "code") <- 
    construct_design_code(process_tracing_designer, match.call.defaults())
  
  process_tracing_design <- set_diagnosands(
    process_tracing_design,
    diagnosands = declare_diagnosands(
      bias = mean(posterior_H - estimand),
      rmse = sqrt(mean((posterior_H - estimand)^2)),
      mean_estimand = mean(estimand),
      mean_posterior = mean(posterior_H),
      sd_posterior = sd(posterior_H),
      keep_defaults = FALSE
    ))
  
  process_tracing_design
}

attr(process_tracing_designer,"shiny_arguments") <- list(
  prior_H = c(.25,.5),
  p_E1_H = c(.3,.8),
  cor_E1E2_H = c(0,.32)
)
attr(process_tracing_designer,"tips") <- c(
  prior_H = "Prior probability that the hypothesis that X causes Y is true.",
  p_E1_H = "Probability of observing the first piece of evidence given X indeed causes Y.",
  cor_E1E2_H = "Correlation in first and second pieces of evidence given X indeed causes Y."
)
attr(process_tracing_designer,"description") <- "A process-tracing design in which two pieces of evidence are sought and used to update about whether X caused Y using Bayes' rule."

