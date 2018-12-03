#' Create a factorial design
#'
#' A \code{2^k} factorial designer with \code{k} factors assigned with independent probabilities. Results in \code{2^k} treatment combinations, each with independent, normally distributed shocks. Estimands are average effects and average interactions of given conditions, averaged over other conditions. Estimation uses regression of demeaned variables with propensity weights. 
#'
#' @param N An integer. Size of sample.
#' @param k An integer. The number of factors in the design.
#' @param outcome_means A numeric vector of length \code{2^k}. Means for each of the \code{2^k} treatment combinations. See `Details` for the correct order of values. 
#' @param sd A nonnegative number. Standard deviation for outcomes when all outcomes have identical standard deviations. For outcome-specific standard deviations use \code{outcomes_sds}.
#' @param outcome_sds A non negative numeric vector of length \code{2^k}. Standard deviations for each of the treatment combinations. See `Details` for the correct order of values. 
#' @param assignment_probs A numeric vector of length \code{k}. Independent probability of assignment to each treatment. 
#' @param outcome_name A character. Name of outcome variable (defaults to "Y"). Must be provided without spacing inside the function \code{c()} as in \code{outcome_name = c("War")}.
#' @param treatment_names A character vector of length \code{k}. Name of treatment factors variable (defaults to "T1", "T2", ..., "Tk"). Must be provided without spacing.
#' @param fixed A character vector. Names of arguments to be fixed in design. By default \code{k}, \code{probs}, \code{outcome_name}, and \code{treatment_names} are always fixed.
#' @return A factorial design.
#' @details 
#' 
#' \code{factorial_designer} creates a factorial design with \code{2^k} treatment combinations resulting from \code{k} factors, each with two conditions each (\code{c(0,1)}). The order of the scalar arguments \code{outcome_means} and \code{outcome_sds} must follow the one returned by \code{expand.grid(rep(list(c(0,1)), k))}, where each of the columns is a treatment. 
#' 
#' Estimands are defined for each combination of treatment assignment as linear combinations of potential outcomes, typically weighted averages of differences. Note that the weighting for the estimand does not reflect treatment assignment probabilities but rather weights each possible condition equally. 
#' 
#' For example, in a design with \eqn{k = 3} factors, the treatment effect of A, (TE_A), averaged over conditions defined by B and C, is given by: \deqn{TE_A = 1/4*(Y_{111} - Y_{011}) + 1/4*(Y_{101} - Y_{001}) + 1/4*(Y_{110} - Y_{010}) + 1/4*(Y_{100} - Y_{000}).} The "average interaction of A and B" --- that is the average effect (for a single unit) of A on the effect of B across conditions defined by C --- is: \deqn{TE_{AB} = 1/2*[(Y_{111} - Y_{011}) - (Y_{101} - Y_{001})] + 1/2*[(Y_{110} - Y_{010}) - (Y_{100} - Y_{000})].} And the triple interaction---that is, the effect of C on the the effect of B on the effect of A  is: \deqn{TE_{ABC} = [(Y_{111} - Y_{011}) - (Y_{101} - Y_{001})] - [(Y_{110} - Y_{010}) - (Y_{100} - Y_{000})],} where \eqn{Y_{abc}} is short for the potential outcome of Y when A is a,  B is b,  and C is c.
#' 
#' Estimates draw from a regression in which all treatments are demeaned and weighted by the inverse probability of being in the condition they are in. 
#' Note that in  this demeaned regression the constant captures the average outcome across all conditions --- not the outcome when all units are in the control condition. The coefficient on T1 captures the average effect of T1 across other conditions---not the effect of T1 when other conditions are at 0. And so on.
#' 
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept factorial
#' @importFrom DeclareDesign declare_assignment declare_estimand declare_estimator declare_population declare_potential_outcomes declare_reveal declare_step diagnose_design tidy_estimator
#' @importFrom fabricatr fabricate fabricate
#' @importFrom randomizr conduct_ra 
#' @importFrom estimatr tidy lm_robust
#' @importFrom rlang eval_bare expr is_integerish parse_expr quo_text quos sym UQS
#' @importFrom stats rnorm formula
#' @export
#' @examples
#' 
#' # A factorial design using default arguments
#' factorial_design <- factorial_designer()
#' 
#' # A 2 x 2 x 2 factorial design with unequal probabilities of assignment to 
#' # each treatment condition. In this case the estimator weights up by the 
#' # conditional probabilities of assignment.
#' factorial_design_2 <- factorial_designer(k = 3, 
#'                                          assignment_probs = c(1/2, 1/4, 1/8), 
#'                                          outcome_means = c(0,0,0,0,0,0,0,4))
#' \dontrun{
#' diagnose_design(factorial_design_2)
#' }
#' # Mapping from outcomes to estimands 
#' # The mapping between the potential outcomes schedule and the estimands of
#' # interest is not always easy. To help with intuition consider a 2^3 
#' # factorial design. You might like to think of a data generating process as
#' # a collection of marginal effects and interaction effects mapping from
#' # treatments to outcomes. 
#' # For instance: Y = -.25 + .75*X1 - .25*X2 -.25*X3 + X1*X2*X3
#' # The vector of implied potential outcome means as a function of conditions  
#' # could then be generated like this:
#' 
#' X <- expand.grid(rep(list(c(0,1)), 3))
#' outcome_means =  -.25 + X[,1]*3/4 - X[,2]/4 - X[,3]/4 + X[,1]*X[,2]*X[,3]
#' outcomes <- cbind(X, outcome_means)
#' colnames(outcomes) <- c("X1", "X2", "X3", "mean")
#' outcomes
#' 
#' # Examination of the outcomes in this table reveals that there is an 
#' # average outcome of 0 (over all conditions), an average effect of treatment
#' # X1 of 1,  an average effects for X2 and X3 of 0,  the two way interactions 
#' # are .5 (averaged over conditions of the third treatment) and the triple 
#' # interaction is 1.
#' # These are exactly the estimands calculated by the designer and returned in 
#' # diagnosis.
#' factorial_design_3 <- factorial_designer(k = 3, 
#'                                          outcome_means = outcome_means,
#'                                          outcome_sds = rep(.01, 8))
#' \dontrun{
#' library(DeclareDesign)
#' diagnose_design(factorial_design_3, sims = 10)
#' }
#' 
#' 
factorial_designer <- function(
  N = 256,
  k = 3,
  outcome_means = rep(0, 2^k),
  sd = 1, 
  outcome_sds = rep(sd, 2^k),
  assignment_probs = rep(.5, k),
  outcome_name = c("Y"),
  treatment_names = NULL,
  fixed = NULL
){
  
  # tests -------------------------------------------------------------------
  
  if(any(grepl(" ", fixed = TRUE, outcome_name))) stop("Please remove spaces from `outcome_name' strings.")
  if(length(outcome_means) != 2^k || length(outcome_sds) != 2^k) stop("`outcome_means' and `outcome_sds` arguments must be the same as length of 2^(k).")
  if(length(assignment_probs) != k) stop("`assignment_probs` must be the same as length of k.")
  if(k < 2 || !is_integerish(k)) stop("`k' should be a positive integer > 1.")
  if(any(outcome_sds<0)) stop("`outcome_sds' should be nonnegative.")
  if(any(assignment_probs <= 0)) stop("`assignment_probs' should have positive values only.")
  
  # pre-objects -------------------------------------------------------------
  
  #names of conditions
  if(is.null(treatment_names)) treatment_names <- paste0("T", 1:k)
  cond_list <- rep(list(c(0,1)),k)
  names(cond_list) <- treatment_names
  cond_grid <- expand.grid(cond_list)
  
  # assignment strings
  # a <- sapply(1:k, function(x) ifelse(cond_grid[,x]==1, paste0(treatment_names[x], "_1"), paste0(treatment_names[x], "_0")))
  a <- sapply(1:k, function(x) ifelse(cond_grid[,x]==1, "1", "0"))
  assignment_string <- sapply(1:2^k, function(r) paste0(a[r,], collapse = "_"))

  # regression term strings
  b <- sapply(1:k, function(x) ifelse(cond_grid[,x]==1, paste0(treatment_names[x]), "-"))
  term_string <- sapply(1:2^k, function(r) paste0(b[r,], collapse = ":"))
  term_string <- gsub("-:|:-", "", term_string)
  term_string[term_string=="-"] <- "(Intercept)"
  
  # probability each treatment combination
  prob_each <- apply(sapply(1:k, function(k){
    assignment_probs[k] * cond_grid[,k] + (1-assignment_probs[k]) * (1-cond_grid[,k])
  }), 1, prod)
  
  cond_row <- lapply(1:k, function(x) which(cond_grid[,x]==1))
  
  # fixed argument ----------------------------------------------------------
  
  outcome_sds_ <- outcome_sds; outcome_means_ <- outcome_means; assignment_probs_ <- assignment_probs; N_ <- N; k_ <- k 
  
  if(is.null(fixed)) fixed <- ""
  if(!"outcome_sds"   %in% fixed)  outcome_sds_ <- sapply(1:length(outcome_sds), function(i) expr(outcome_sds[!!i])) 
  if(!"outcome_means" %in% fixed)  outcome_means_ <- sapply(1:length(outcome_means), function(i) expr(outcome_means[!!i])) 
  if(!"N" %in% fixed)  N_ <- expr(N)
  
  
  # population --------------------------------------------------------------
  population_expr <- expr(declare_population(!!N_))
  
  # potential outcomes ------------------------------------------------------
  potouts <- sapply(1:length(outcome_means),
                    function(i) quos(!!outcome_means_[[i]] + rnorm(!!N_, 0, !!outcome_sds_[[i]])))
  names_pos <- paste0(outcome_name, "_", assignment_string)
  names(potouts) <- names_pos
  
  potential_outcomes_expr <- expr(declare_potential_outcomes(!!!(potouts)))
  
  # assignment --------------------------------------------------------------
  Z <- sym("Z")
  assignment_given_factor <- sapply(1:length(cond_row), function(i) quos(as.numeric(!!Z %in% !!cond_row[[i]])))
  names(assignment_given_factor) <- treatment_names
  
  assignment_expr1 <- expr(declare_assignment(conditions = 1:(2^!!k_), prob_each = !!prob_each))
  assignment_expr2 <- expr(declare_step(fabricate, !!!assignment_given_factor))
  
  # reveal outcomes ---------------------------------------------------------
  reveal_expr <- expr(declare_reveal(
    handler = function(data){
      potential_cols <- mapply(paste, data[, !!treatment_names, drop = FALSE], sep = "_", SIMPLIFY = FALSE)
      potential_cols <- do.call(paste, c(!!outcome_name, potential_cols, sep = "_"))
      upoc <- unique(potential_cols)
    
      df <- data[, upoc, drop = FALSE]
      R <- seq_len(nrow(df))
      C <- match(potential_cols, colnames(df))
      data[,!!outcome_name] <- df[cbind(R, C)]
      data
    }))
  
  # estimands ---------------------------------------------------------------
  perm <- function(v) {
    sapply(1:length(v), function(x) {
      rep( rep(1:v[x], each=prod(v[x:length(v)]) / v[x]),
           length.out=prod(v))
    } ) - 1
  }
  
  interaction  <- function(k, tnames = treatment_names, yname = outcome_name) {
    conditions <- perm(rep(2,k))
    # combs <- paste0(yname, "_", apply(conditions, 1, function(x) paste0(tnames, "_", x, collapse = "_")))
    combs <- paste0(yname, "_", apply(conditions, 1, function(x) paste0(x, collapse = "_")))
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
    names(out) <-  c("PO", tnames, "Overall_average", 
                     apply(conditions  ==1, 1, function(r) paste0("te_", paste(tnames[r], collapse = ":")))[-1])
    return(out)
  }
  
  d <- interaction(k)
  estimand_operations <- apply(d[,(k+2):ncol(d)], 2, function(col) paste(col, "*", d$PO, collapse = " + ")) 
  estimand_preexpr <- sapply(1:2^k, function(i) expr(mean(!!parse_expr(estimand_operations[i]))))
  names(estimand_preexpr) <-  names(estimand_operations)
 
  estimand_expr <- expr(declare_estimand(UQS(estimand_preexpr), label = "ATE"))
  
  # estimators --------------------------------------------------------------
  estimator_formula <- formula(paste0(outcome_name, " ~ ", paste(treatment_names, collapse = "*")))
  
  estimator_expr <- expr(
    declare_estimator(
      handler = tidy_estimator(function(data){
        data[, names(data) %in% !!treatment_names] <- data[, names(data) %in% !!treatment_names] - 0.5
        mod <- lm_robust(formula = !!estimator_formula, data = data, weights = 1/Z_cond_prob)
        estimate_df <- tidy(mod)
        estimate_df$estimand_label <- paste0("te_", estimate_df$term)
        estimate_df$estimand_label[estimate_df$estimand_label == "te_(Intercept)"] <- "Overall_average"
        estimate_df
      })))
  
  # design code -------------------------------------------------------------
  
  {{{
    
    # M: Model
    population <- eval_bare(population_expr)
    
    potential_outcomes <- eval_bare(potential_outcomes_expr)
    
    reveal_Y <- eval_bare(reveal_expr)
    
    # I: Inquiry
    estimand <- eval_bare(estimand_expr)
    
    # D: Data Strategy
    assignment_factors <- eval_bare(assignment_expr1)
    
    assignment <- eval_bare(assignment_expr2)
    
    # A: Answer Strategy
    estimator <- eval_bare(estimator_expr)
    
    # Design
    factorial_design <- population + potential_outcomes + assignment_factors + 
      assignment + reveal_Y + estimand + estimator
    
  }}}
  
  design_code <- construct_design_code(factorial_designer,
                                       match.call.defaults(),
                                       # rlang = TRUE,
                                       arguments_as_values = TRUE,
                                       exclude_args = c("k", "assignment_probs", "outcome_name", "treatment_names", "sd", fixed, "fixed"))
  
  
  design_code <-
    gsub("eval_bare\\(population_expr\\)", quo_text(population_expr), design_code)
  design_code <-
    gsub("eval_bare\\(potential_outcomes_expr\\)", quo_text(potential_outcomes_expr), design_code)
  design_code <-
    gsub("eval_bare\\(reveal_expr\\)", quo_text(reveal_expr), design_code)
  design_code <-
    gsub("eval_bare\\(estimand_expr\\)", quo_text(estimand_expr), design_code)
  design_code <- 
    gsub("eval_bare\\(assignment_expr1\\)", quo_text(assignment_expr1), design_code)
  design_code <- 
    gsub("eval_bare\\(assignment_expr2\\)", quo_text(assignment_expr2), design_code)
  design_code <- 
    gsub("eval_bare\\(estimator_expr\\)", quo_text(estimator_expr), design_code)
  
  attr(factorial_design, "code") <- design_code
  
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
<p> A <code>2^k</code> factorial designer with <code>k</code> factors assigned with independent
probabilities. Results in <code>2^k</code> treatment combinations, each with independent,
normally distributed shocks.
"
