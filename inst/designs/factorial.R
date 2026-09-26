---
id: factorial
label: Factorial experiment
category: template
keywords: [experiment, causal, factorial, interaction]
description: >
  A 2^k factorial experiment. Each of k factors is assigned with its own
  probability, giving 2^k treatment combinations, and each combination has its
  own normally distributed potential outcome. The inquiries are the overall
  average, the average effect of each factor, and every interaction, each
  averaged with equal weight over the conditions of the other factors (not
  weighted by assignment probabilities). The estimator regresses the outcome on
  the full interaction of the treatments centered at 0.5, weighted by the
  inverse probability of the assigned combination. outcome_means and
  outcome_sds follow the row order of expand.grid(rep(list(0:1), k)), in which
  the first factor varies fastest. Redesigning k resets the length-dependent
  defaults (outcome_means, outcome_sds, assignment_probs, treatment_names) only
  when they are left at their defaults; a supplied vector of the wrong length
  for k is an error.
  Flattened from DesignLibrary::factorial_designer.
params:
  "N": "Number of units"
  "k": "Number of factors"
  "sd": "Standard deviation of every potential outcome, when outcome_sds is not given"
  "outcome_means": "Mean outcome in each of the 2^k treatment combinations"
  "outcome_sds": "Standard deviation of the outcome in each of the 2^k treatment combinations"
  "assignment_probs": "Probability of assignment to each factor (length k)"
  "outcome_name": "Name of the outcome variable"
  "treatment_names": "Names of the k factors"
include_in_shiny: true
---

# Every step is a function of k, so that redesigning k rebuilds the potential
# outcomes, the factor indicators, and the 2^k inquiries without changing the
# steps. Each helper takes the parameters it needs as arguments named for them:
# a step reaches a parameter only through an argument of the same name.

# Treatment combinations in expand.grid order, first factor fastest; row i is
# condition Z = i.
factorial_grid <- function(k, treatment_names) {
  if (length(treatment_names) != k) {
    stop("treatment_names has length ", length(treatment_names), " but k is ", k, ".", call. = FALSE)
  }
  grid <- expand.grid(rep(list(c(0, 1)), k))
  names(grid) <- treatment_names
  grid
}

# Potential outcome columns in the fabricatr convention, e.g. Y_T1_0_T2_1_T3_0
factorial_po_names <- function(k, outcome_name, treatment_names) {
  grid <- factorial_grid(k, treatment_names)
  cells <- apply(grid, 1, function(r) paste0(treatment_names, "_", r, collapse = "_"))
  paste0(outcome_name, "_", cells)
}

factorial_population <- function(data, N, k, outcome_means, outcome_sds,
                                  outcome_name, treatment_names) {
  for (x in c("outcome_means", "outcome_sds")) {
    n_x <- length(get(x))
    if (n_x != 2^k) stop(x, " has length ", n_x, " but 2^k is ", 2^k, ".", call. = FALSE)
  }
  pop <- fabricate(N = N)
  po <- factorial_po_names(k, outcome_name, treatment_names)
  for (i in seq_len(2^k)) {
    pop[[po[i]]] <- outcome_means[i] + rnorm(N, 0, outcome_sds[i])
  }
  pop
}

factorial_assignment <- function(data, k, assignment_probs, treatment_names) {
  if (length(assignment_probs) != k) {
    stop("assignment_probs has length ", length(assignment_probs), " but k is ", k, ".", call. = FALSE)
  }
  grid <- factorial_grid(k, treatment_names)
  prob_each <- apply(sapply(seq_len(k), function(j) {
    assignment_probs[j] * grid[, j] + (1 - assignment_probs[j]) * (1 - grid[, j])
  }), 1, prod)
  data$Z <- complete_ra(nrow(data), conditions = 1:(2^k), prob_each = prob_each)
  data$Z_cond_prob <- obtain_condition_probabilities(
    assignment = data$Z, conditions = 1:(2^k), prob_each = prob_each
  )
  for (j in seq_len(k)) {
    data[[treatment_names[j]]] <- as.numeric(data$Z %in% which(grid[, j] == 1))
  }
  data
}

factorial_reveal <- function(data, k, outcome_name, treatment_names) {
  po <- as.matrix(data[factorial_po_names(k, outcome_name, treatment_names)])
  data[[outcome_name]] <- po[cbind(seq_len(nrow(data)), data$Z)]
  data
}

# Each inquiry is a signed, equally weighted sum of the 2^k potential outcomes.
# Rows of `conditions` enumerate the combinations with the LAST factor fastest;
# the weights for row j give the interaction of the factors that are 1 in j.
factorial_inquiries <- function(data, k, outcome_name, treatment_names) {
  conditions <- sapply(seq_len(k), function(x) {
    rep(rep(1:2, each = 2^(k - x)), length.out = 2^k)
  }) - 1
  po <- paste0(outcome_name, "_", apply(conditions, 1, function(x) {
    paste0(treatment_names, "_", x, collapse = "_")
  }))
  signs <- (1 - 2 * (k %% 2)) * (1 - 2 * rowSums(conditions) %% 2)
  weights <- sapply(seq_len(2^k - 1), function(j) {
    others <- t(t(conditions) * (1 - conditions[j, ]))
    selection <- 2 * (rowSums(others) %% 2) - 1
    if (sum(1 - conditions[j, ]) %% 2 == 0) selection <- -selection
    signs * selection * 0.5^(k - sum(conditions[j, ]))
  })
  weights <- cbind(weights, signs)
  po_values <- as.matrix(data[po])
  data.frame(
    inquiry = c("Overall_average", apply(conditions == 1, 1, function(r) {
      paste0("te_", paste(treatment_names[r], collapse = ":"))
    })[-1]),
    estimand = colMeans(po_values %*% weights),
    row.names = NULL
  )
}

# Treatments centered at 0.5, so the intercept is the average over all
# combinations and each coefficient an effect averaged over the other factors
factorial_estimator <- function(data, outcome_name, treatment_names) {
  data[treatment_names] <- data[treatment_names] - 0.5
  fit <- lm_robust(
    formula(paste0(outcome_name, " ~ ", paste(treatment_names, collapse = "*"))),
    data = data,
    weights = 1 / Z_cond_prob
  )
  estimates <- tidy(fit)
  estimates$inquiry <- paste0("te_", estimates$term)
  estimates$inquiry[estimates$inquiry == "te_(Intercept)"] <- "Overall_average"
  estimates
}

design <-
  declare_parameters(
    N = 256,
    k = 3,
    sd = 1,
    outcome_means = rep(0, 2^k),
    outcome_sds = rep(sd, 2^k),
    assignment_probs = rep(0.5, k),
    outcome_name = "Y",
    treatment_names = paste0("T", seq_len(k))
  ) +
  declare_model(
    handler = factorial_population,
    N = N,
    k = k,
    outcome_means = outcome_means,
    outcome_sds = outcome_sds,
    outcome_name = outcome_name,
    treatment_names = treatment_names
  ) +
  declare_assignment(
    handler = factorial_assignment,
    k = k,
    assignment_probs = assignment_probs,
    treatment_names = treatment_names
  ) +
  declare_measurement(
    handler = factorial_reveal,
    k = k,
    outcome_name = outcome_name,
    treatment_names = treatment_names
  ) +
  declare_inquiry(
    handler = factorial_inquiries,
    k = k,
    outcome_name = outcome_name,
    treatment_names = treatment_names
  ) +
  declare_estimator(
    handler = factorial_estimator,
    outcome_name = outcome_name,
    treatment_names = treatment_names
  )
