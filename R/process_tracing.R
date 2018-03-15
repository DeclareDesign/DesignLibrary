#' @export
process_tracing_template <- function(
  N = c(200,100,300,500,1000),
  X_prevalence_rate = c(.7,seq(.1,.9,.1)),
  K_prevalence_rate = c(.2,seq(.1,.9,.1))
)
{
  {
    N <- as.numeric(N[1])
    X_prevalence_rate <- as.numeric(X_prevalence_rate[1])
    K_prevalence_rate <- as.numeric(K_prevalence_rate[1])
  }
  {{{
    population <- declare_population(
      N =  200,
      type = sample(c('A','B','C','D'), N, TRUE),
      X = rbinom(N, 1, X_prevalence_rate),
      K = ifelse(X == 1 & type == 'B', rbinom(1, 1, K_prevalence_rate), 0),
      Y = (type == 'A' & !X) | (type == 'B' & X) | (type == 'D'))
    sampling <- declare_sampling(
      handler = function(data) {
        eligible_cases <- with(data, which(X & Y))
        return(data[sample(eligible_cases, 1), ])})
    estimand <- declare_estimand(is_B = type == 'B')
    estimator <- declare_estimator(
      handler = tidy_estimator(function(data) {
        with(data, data.frame(guess = ifelse(K, 1, .5), K_seen = K == 1))}),
      estimand = estimand)
    process_tracing <- declare_design(
      population, sampling, estimand, estimator)
  }}}
  process_tracing
}

attr(process_tracing_template, "tips") <- c(
  N = "Size of sample",
  X_prevalence_rate = "Prevalence of explanatory variable among cases",
  K_prevalence_rate = "Probability of observing clue, given X is observed"
)








