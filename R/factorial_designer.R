#' Create a factorial design
#'
#' A \code{2^k} factorial designer.
#'
#' @param N An integer. Size of sample.
#' @param k An integer. The number of factors in the design.
#' @param means A numeric vector. Means for each of the \code{2^k} treatment combinations. See `Details` for the correct order of values. 
#' @param sds A numeric vector. Standard deviations for each of the \code{2^k} treatment combinations. See `Details` for the correct order of values. 
#' @param probs A numeric vector of length \code{k}. Independent probability of assignment to each treatment. 
#' @return A factorial design.
#' @details 
#' 
#' \code{factorial_designer} creates a factorial design with \code{2^k} treatment combinations resulting from \code{k} factors, each with two conditions each (\code{c(0,1)}). The order of the scalar arguments \code{means} and \code{sds} must follow the one returned by \code{expand.grid(rep(list(c(0,1)), k))}, where each of the columns is a treatment
#' 
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept factorial
#' @export
#' @examples
#' # A factorial design using default arguments
#' factorial_design <- factorial_designer()
#' 
#' # A 2 x 2 x 2 factorial design with unequal probabilities of assignment
#' # to each treatment
#' factorial_design2 <- factorial_designer(k = 3, probs = c(1/2, 1/4, 1/4))
#'

factorial_designer <- function(
  N = 500,
  k = 2,
  means = seq(0:.5, length.out = 2^k),
  sds = rep(.1, 2^k),
  probs = rep(.5, k),
  compare_with = NULL,
  fixed = NULL
){
  
  # tests -------------------------------------------------------------------
  
  if(length(means) != 2^k || length(sds) != 2^k) stop("`means' and `sds` arguments must be the same as length of 2^(k).")
  if(length(probs) != k) stop("`probs` must be the same as length of k.")
  if(k <= 0 || !rlang::is_integerish(k)) stop("`k' should be a positive integer.")
  if(any(sds<=0)) stop("`sds' should be positive.")
  if(any(probs <= 0)) stop("`probs' should have positive values only.")
  
  # pre-objects -------------------------------------------------------------
  
  #names of conditions
  cond_names <- paste0("T", 1:k)
  cond_list <- rep(list(c(0,1)),k)
  names(cond_list) <- cond_names
  cond_grid <- expand.grid(cond_list)
  
  # assignment strings
  a <- sapply(1:k, function(x) ifelse(cond_grid[,x]==1, paste0("T", x, "_1"), paste0("T", x, "_0")))
  assignment_string <- sapply(1:2^k, function(r) paste0(a[r,], collapse = "_"))
  
  # regression term strings
  b <- sapply(1:k, function(x) ifelse(cond_grid[,x]==1, paste0("T", x), "-"))
  term_string <- sapply(1:2^k, function(r) paste0(b[r,], collapse = ":"))
  term_string <- gsub("-:|:-", "", term_string)
  term_string[term_string=="-"] <- "(None)"
  
  # probability each treatment combination
  prob_each <- apply(sapply(1:k, function(k){
    probs[k] * cond_grid[,k] + (1-probs[k]) * (1-cond_grid[,k])
  }), 1, prod)
  
  cond_row <- lapply(1:k, function(x) which(cond_grid[,x]==1))
  cond <- sapply(1:k, function(x) ifelse(cond_grid[,x]==1, paste0("T", x, "==1"), paste0("T", x, "==0")))
  cond_logical <- sapply(1:2^k, function(r) paste0(cond[r,], collapse = " & "))
  
  if(is.null(compare_with)) compare_with <- assignment_string[1]
  
  # population --------------------------------------------------------------
  
  # potential outcomes ------------------------------------------------------
  
  potouts <- sapply(1:length(means),
                    function(i) rlang::quos(means[!!i] + rnorm(N, 0, sds[!!i])))
  names_pos <- paste0("Y_", assignment_string)
  names(potouts) <- names_pos
  
  # pos <- rlang::expr(declare_potential_outcomes(rlang::UQS(potouts), assignment_variables = !!cond_names))
  
  # assignment --------------------------------------------------------------
  assignment_given_factor <- sapply(1:length(cond_row), function(i) rlang::quos(as.numeric(Z %in% !!cond_row[[i]])))
  names(assignment_given_factor) <- cond_names
  
  # reveal outcomes ---------------------------------------------------------
  
  # estimands ---------------------------------------------------------------
  Y_compare_with <- paste0("Y_", compare_with)
  estimand_expr <- sapply(1:length(term_string), function(i) rlang::expr(mean(!!rlang::sym(names_pos[i]) - !!rlang::sym(Y_compare_with))))
  names(estimand_expr) <- term_string
  
  # estimators --------------------------------------------------------------
  estimator_formula <- formula(paste0("Y ~ ", paste(cond_names, collapse = "*")))
  
  # design code -------------------------------------------------------------
  
  {{{
    
    # M: Model
    population <- declare_population(N)
    
    pos <- rlang::quo(
      declare_potential_outcomes(!!!(potouts))
    )
    
    reveal_Y <- rlang::quo(
      declare_reveal(outcome_variables = "Y", assignment_variables = !!cond_names)
    )
    
    # I: Inquiry
    estimand <- rlang::quo(
      declare_estimand(rlang::UQS(estimand_expr))
    )
    
    # D: Data Strategy
    assignment_factors <- declare_assignment(conditions = 1:(2^k), prob_each = prob_each)
    assignment <- rlang::quo(
      declare_step(fabricate, !!!assignment_given_factor)
    )
    
    # A: Answer Strategy
    estimator_function <- rlang::quo(
      function(data){
        mod = lm_robust(formula = !!estimator_formula, data = data, weights = 1/(data$Z_cond_prob))
        df = data.frame(predict(mod, newdata=!!expand.grid(cond_list), se.fit = TRUE, weights = 1/prob_each, interval = "confidence"))
        #calculate differences in fitted values
        comparison = which(!!assignment_string == !!compare_with)
        df_estimator = data.frame(term = !!term_string,
                                  estimate = df$fit.fit - df$fit.fit[comparison],
                                  std.error = sqrt((1/prob_each)*df$se.fit^2 + (1/prob_each[comparison])* df$se.fit[comparison]^2))
        df_estimator$conf.low = with(df_estimator, estimate-1.96*std.error)
        df_estimator$conf.high = with(df_estimator, estimate+1.96*std.error)
        df_estimator$df = mod$df
        df_estimator
      }
    )
    
    estimator <- rlang::quo(
      declare_estimator(
        handler = tidy_estimator(rlang::eval_bare(estimator_function)),
        estimand = !!term_string)
    )
    
    # Design
    factorial_design <- population + rlang::eval_tidy(pos) + 
      assignment_factors + rlang::eval_tidy(assignment) +
      rlang::eval_tidy(reveal_Y) + rlang::eval_tidy(estimand) + rlang::eval_tidy(estimator)
    
  }}}
  
  attr(factorial_design, "code") <- construct_design_code(factorial_designer,
                                                          match.call.defaults(),
                                                          rlang = TRUE)
  
  return(factorial_design)
  
}

attr(factorial_designer,"shiny_arguments") <-
  list(
    N = c(50, 100, 500, 1000),
    k = c(2, 3, 4)
  )

attr(factorial_designer,"tips") <-
  c(N = "Size of sample",
    k = "The number of factors in the design"
  )

attr(factorial_designer,"description") <- "
<p> A <code>2^k</code> factorial design with sample size <code>N</code>
and <code>k</code> treatment factors.
"










