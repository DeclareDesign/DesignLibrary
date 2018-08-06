#' Create a factorial design
#'
#' A \code{2^k} factorial designer.
#'
#' @param N An integer. Size of sample.
#' @param k An integer. The number of factors in the design.
#' @param means A numeric vector of length \code{2^k}. Means for each of the \code{2^k} treatment combinations. See `Details` for the correct order of values. 
#' @param sds A numeric vector of length \code{2^k}. Standard deviations for each of the treatment combinations. See `Details` for the correct order of values. 
#' @param probs A numeric vector of length \code{k}. Independent probability of assignment to each treatment. 
#' @return A factorial design.
#' @details 
#' 
#' \code{factorial_designer} creates a factorial design with \code{2^k} treatment combinations resulting from \code{k} factors, each with two conditions each (\code{c(0,1)}). The order of the scalar arguments \code{means} and \code{sds} must follow the one returned by \code{expand.grid(rep(list(c(0,1)), k))}, where each of the columns is a treatment.
#' 
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept factorial
#' @export
#' @import rlang
#' @examples
#' 
#' # A factorial design using default arguments
#' factorial_design <- factorial_designer()
#' 
#' # A 2 x 2 x 2 factorial design with unequal probabilities of assignment
#' # to each treatment
#' factorial_design2 <- factorial_designer(k = 3, probs = c(1/2, 1/4, 1/4))
#'

factorial_designer <- function(
  N = 500,
  k = 3,
  means = seq(0:.5, length.out = 2^k),
  sds = rep(.1, 2^k),
  probs = rep(.5, k),
  outcome_name = c("Y"),
  treatment_names = NULL,
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
  if(is.null(treatment_names)) treatment_names <- paste0("T", 1:k)
  cond_list <- rep(list(c(0,1)),k)
  names(cond_list) <- treatment_names
  cond_grid <- expand.grid(cond_list)
  
  # assignment strings
  a <- sapply(1:k, function(x) ifelse(cond_grid[,x]==1, paste0(treatment_names[x], "_1"), paste0(treatment_names[x], "_0")))
  assignment_string <- sapply(1:2^k, function(r) paste0(a[r,], collapse = "_"))
  # assignment_string <- sapply(1:2^k, function(r) paste0(cond_grid[r,], collapse = ""))
  
  # regression term strings
  b <- sapply(1:k, function(x) ifelse(cond_grid[,x]==1, paste0(treatment_names[x]), "-"))
  term_string <- sapply(1:2^k, function(r) paste0(b[r,], collapse = ":"))
  term_string <- gsub("-:|:-", "", term_string)
  term_string[term_string=="-"] <- "(Intercept)"
  
  # probability each treatment combination
  prob_each <- apply(sapply(1:k, function(k){
    probs[k] * cond_grid[,k] + (1-probs[k]) * (1-cond_grid[,k])
  }), 1, prod)
  
  cond_row <- lapply(1:k, function(x) which(cond_grid[,x]==1))
  
  # fixed argument ----------------------------------------------------------
  
  sds_ <- sds; means_ <- means; probs_ <- probs; N_ <- N; k_ <- k 
  
  if(is.null(fixed)) fixed <- ""
  if(!"sds"   %in% fixed)  sds_ <- sapply(1:length(sds), function(i) rlang::expr(sds[!!i])) 
  if(!"means" %in% fixed)  means_ <- sapply(1:length(means), function(i) rlang::expr(means[!!i])) 
  if(!"N"     %in% fixed)  N_ <- rlang::expr(N)
  
  
  # population --------------------------------------------------------------
  
  # potential outcomes ------------------------------------------------------
  
  potouts <- sapply(1:length(means),
                    function(i) rlang::quos(!!means_[[i]] + rnorm(!!N_, 0, !!sds_[[i]])))
  names_pos <- paste0(outcome_name, "_", assignment_string)
  names(potouts) <- names_pos
  
  # assignment --------------------------------------------------------------
  assignment_given_factor <- sapply(1:length(cond_row), function(i) rlang::quos(as.numeric(Z %in% !!cond_row[[i]])))
  names(assignment_given_factor) <- treatment_names
  
  # reveal outcomes ---------------------------------------------------------
  
  # estimands ---------------------------------------------------------------
  
  perm <- function(v) {
    sapply(1:length(v), function(x) {
      rep( rep(1:v[x], each=prod(v[x:length(v)]) / v[x]),
           length.out=prod(v))
    } ) - 1
  }
  
  interaction  <- function(k, tnames = treatment_names, yname = outcome_name) {
    if(k < 2) stop("k > 1 please")
    conditions <- perm(rep(2,k))
    combs <- paste0(yname, "_", apply(conditions, 1, function(x) paste0(tnames, "_", x, collapse = "_")))
    signs <- (1 - 2*(k%%2))*( 1- 2*apply(conditions, 1, sum) %% 2)
    
    allsigns <- sapply(1:((nrow(conditions)-1)), function(j) {
      others <- t(t(conditions) * (1-conditions[j,])) # Values of xx in YxxXX given XX
      selection <- apply(others, 1, sum)%% 2 # Set of YxxXX combinations in which sum(xx) is odd given XX
      selection <- 2*selection - 1  # keep odds, reverse evens
      if(sum(1-conditions[j,]) %% 2 == 0)  selection <- -selection
      signs*selection * .5^(k-sum(conditions[j,])) # modify signs and weight 
    })
    
    out <- data.frame(combs,conditions,  allsigns, signs)
    
    # if(is.null(tnames)) tnames <- 1:k
    names(out) <-  c("PO", tnames, "Control", 
                     apply(conditions  ==1, 1, function(r) paste0("coef_", paste(tnames[r], collapse = ":")))[-1])
    return(out)
  }
  
  d <- interaction(k)
  estimand_operations <- apply(d[,(k+2):ncol(d)], 2, function(col) paste(col, "*", d$PO, collapse = " + ")) 
  estimand_expr <- sapply(1:2^k, function(i) rlang::expr(mean(!!rlang::parse_expr(estimand_operations[i]))))
  names(estimand_expr) <-  names(estimand_operations)
  
  # estimators --------------------------------------------------------------
  estimator_formula <- formula(paste0(outcome_name, " ~ ", paste(treatment_names, collapse = "*")))
  
  # design code -------------------------------------------------------------
  
  {{{
    
    # M: Model
    population <- rlang::quo(
      declare_population(!!N_)
    )
    
    potentials <- rlang::quo(
      declare_potential_outcomes(!!!(potouts))
    )
    
    reveal_Y <- rlang::quo(
      declare_reveal(outcome_variables = !!outcome_name, assignment_variables = !!treatment_names)
    )
    
    # I: Inquiry
    estimand <- rlang::quo(
      declare_estimand(rlang::UQS(estimand_expr), label = "ATE")
    )
    
    # D: Data Strategy
    assignment_factors <- rlang::quo(
      declare_assignment(conditions = 1:(2^!!k_), prob_each = !!prob_each)
    )
    
    assignment <- rlang::quo(
      declare_step(fabricate, !!!assignment_given_factor)
    )
    
    # A: Answer Strategy
    estimator_function <- rlang::quo(
      function(data){
        data[, names(data) %in% !!treatment_names] <- data[, names(data) %in% !!treatment_names] - 0.5
        estimate_df <- tidy.lm_robust(lm_robust(formula = !!estimator_formula, data = data, weights = 1/(data$Z_cond_prob)))
        # estimate_df$estimate_term <- estimate_df$term
        estimate_df$estimand_label <- paste0("coef_", estimate_df$term)
        estimate_df$estimand_label[estimate_df$estimand_label == "coef_(Intercept)"] <- "Control"
        estimate_df
      }
    )
    
    #remove `rlang::eval_bare` call when running code below
    estimator <- rlang::quo(
      declare_estimator(
        handler = tidy_estimator(rlang::eval_bare(estimator_function)))
    )
    
    # Design
    factorial_design <- rlang::eval_tidy(population) + rlang::eval_tidy(potentials) + 
      rlang::eval_tidy(assignment_factors) + rlang::eval_tidy(assignment) +
      rlang::eval_tidy(reveal_Y) + rlang::eval_tidy(estimand) + rlang::eval_tidy(estimator)
    
  }}}
  
  attr(factorial_design, "code") <- construct_design_code(factorial_designer,
                                                          match.call.defaults(),
                                                          rlang = TRUE,
                                                          arguments_as_values = TRUE,
                                                          exclude_args = c("k", "probs", "outcome_name", fixed, "fixed"))
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