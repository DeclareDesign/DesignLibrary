#' @noRd
warn_args_to_fix <- function(args_to_fix) {
  if (!is.null(args_to_fix) && length(args_to_fix) && any(nzchar(as.character(args_to_fix)))) {
    warning(
      "args_to_fix is ignored; parameters remain redesignable on the design object.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' @noRd
call_library_design <- function(id, dots) {
  dots$args_to_fix <- NULL
  do.call(make_design, c(list(design = id), dots))
}

#' Create a one-level two-arm design
#'
#' Routes to [make_design()] with id `"two_arm_flexible"`:
#' `make_design("two_arm_flexible", N = N, assignment_prob = assignment_prob, ...)`.
#'
#' Builds a design with one treatment and one control arm. Treatment effects
#' can be specified by `ate` or by `treatment_mean` (which overrides `ate`).
#' Argument names match DesignLibrary `two_arm_designer`.
#'
#' @param N Sample size.
#' @param assignment_prob Probability of assignment to treatment.
#' @param control_mean Average outcome in control.
#' @param control_sd Standard deviation in control.
#' @param ate Average treatment effect.
#' @param treatment_mean Average outcome in treatment. If supplied, overrides
#'   `ate` (`ate` becomes `treatment_mean - control_mean`).
#' @param treatment_sd Standard deviation in treatment. Defaults to `control_sd`.
#' @param rho Correlation between treatment and control potential outcomes.
#' @param args_to_fix Ignored. Present for DesignLibrary compatibility.
#'
#' @return A design object.
#' @seealso [make_design()]
#' @export
#' @examples
#' \dontrun{
#' make_design("two_arm_flexible", N = 40, ate = 0.2)
#' two_arm_designer(N = 40, ate = 0.2)
#' }
two_arm_designer <- function(
  N = 100,
  assignment_prob = 0.5,
  control_mean = 0,
  control_sd = 1,
  ate = 1,
  treatment_mean = NULL,
  treatment_sd = NULL,
  rho = 1,
  args_to_fix = NULL
) {
  warn_args_to_fix(args_to_fix)
  if (is.null(treatment_sd)) treatment_sd <- control_sd
  if (!is.null(treatment_mean)) ate <- treatment_mean - control_mean
  call_library_design("two_arm_flexible", list(
    N = N,
    assignment_prob = assignment_prob,
    control_mean = control_mean,
    control_sd = control_sd,
    ate = ate,
    treatment_sd = treatment_sd,
    rho = rho
  ))
}

#' Create a two-arm design with attrition
#'
#' Routes to [make_design()] with id `"two_arm_attrition"`:
#' `make_design("two_arm_attrition", N = N, a_R = a_R, ...)`.
#'
#' @inheritParams two_arm_designer
#' @param a_R Constant in the equation relating treatment to reporting.
#' @param b_R Slope relating treatment to reporting.
#' @param a_Y Constant in the equation relating treatment to the outcome.
#' @param b_Y Slope relating treatment to the outcome.
#' @param rho Correlation between reporting and outcome shocks (0 to 1).
#' @return A design object.
#' @seealso [make_design()]
#' @export
two_arm_attrition_designer <- function(
  N = 100,
  a_R = 0,
  b_R = 1,
  a_Y = 0,
  b_Y = 1,
  rho = 0,
  args_to_fix = NULL
) {
  warn_args_to_fix(args_to_fix)
  call_library_design("two_arm_attrition", list(
    N = N, a_R = a_R, b_R = b_R, a_Y = a_Y, b_Y = b_Y, rho = rho
  ))
}

#' Create a pretest-posttest design
#'
#' Routes to [make_design()] with id `"pretest_posttest"`:
#' `make_design("pretest_posttest", N = N, ate = ate, ...)`.
#'
#' @inheritParams two_arm_designer
#' @param sd_1 Standard deviation of the pretest shock.
#' @param sd_2 Standard deviation of the posttest shock.
#' @param rho Correlation between pretest and posttest shocks (-1 to 1).
#' @param attrition_rate Share of units not observed at the posttest.
#' @return A design object.
#' @seealso [make_design()]
#' @export
pretest_posttest_designer <- function(
  N = 100,
  ate = 0.25,
  sd_1 = 1,
  sd_2 = 1,
  rho = 0.5,
  attrition_rate = 0.1,
  args_to_fix = NULL
) {
  warn_args_to_fix(args_to_fix)
  call_library_design("pretest_posttest", list(
    N = N, ate = ate, sd_1 = sd_1, sd_2 = sd_2, rho = rho,
    attrition_rate = attrition_rate
  ))
}

#' Create a randomized response design
#'
#' Routes to [make_design()] with id `"randomized_response"`:
#' `make_design("randomized_response", N = N, ...)`.
#'
#' @inheritParams two_arm_designer
#' @param prob_forced_yes Probability a respondent is forced to answer yes.
#' @param prevalence_rate Share of units holding the sensitive trait.
#' @param withholding_rate Share of trait holders who deny it under direct
#'   questioning.
#' @return A design object.
#' @seealso [make_design()]
#' @export
randomized_response_designer <- function(
  N = 1000,
  prob_forced_yes = 0.6,
  prevalence_rate = 0.1,
  withholding_rate = 0.5,
  args_to_fix = NULL
) {
  warn_args_to_fix(args_to_fix)
  call_library_design("randomized_response", list(
    N = N,
    prob_forced_yes = prob_forced_yes,
    prevalence_rate = prevalence_rate,
    withholding_rate = withholding_rate
  ))
}

#' Create a mediation analysis design
#'
#' Routes to [make_design()] with id `"mediation_analysis"`:
#' `make_design("mediation_analysis", N = N, a = a, ...)`.
#'
#' @inheritParams two_arm_designer
#' @param a Effect of treatment on the latent index for the mediator.
#' @param b Effect of the mediator on the outcome when Z = 0.
#' @param c Interaction of the mediator and treatment in the outcome.
#' @param d Direct effect of treatment on the outcome when M = 0.
#' @param rho Correlation between mediator and outcome shocks (-1 to 1).
#' @return A design object.
#' @seealso [make_design()]
#' @export
mediation_analysis_designer <- function(
  N = 200,
  a = 1,
  b = 0.4,
  c = 0,
  d = 0.5,
  rho = 0,
  args_to_fix = NULL
) {
  warn_args_to_fix(args_to_fix)
  call_library_design("mediation_analysis", list(
    N = N, a = a, b = b, c = c, d = d, rho = rho
  ))
}

#' Create a multi-arm design
#'
#' Routes to [make_design()] with id `"multiarm_trial"`:
#' `make_design("multiarm_trial", N = N, m_arms = m_arms, ...)`.
#' Defaults for `outcome_means`, `outcome_sds`, and `conditions` are
#' evaluated after `m_arms`, so `multi_arm_designer(m_arms = 4)` expands
#' those vectors before calling `make_design()`. `make_design("multiarm_trial",
#' m_arms = 4)` does the same inside the design: constant vectors are repeated
#' and conditions `1..k` become `1..m_arms`. Any other wrong-length vector
#' errors.
#' Inquiries are `mean(Y(k) - Y(1))`; estimation is one `lm_robust`.
#' Library-file knobs match these formals.
#'
#' @inheritParams two_arm_designer
#' @param m_arms Number of arms. May vary. Must match `length(outcome_means)`
#'   unless `outcome_means` is a scalar (then recycled).
#' @param outcome_means Average outcome in each arm. Default `rep(0, m_arms)`.
#' @param sd_i Standard deviation of the individual-level shock.
#' @param outcome_sds Extra standard deviation in each arm. Default
#'   `rep(0, m_arms)`.
#' @param conditions Assignment conditions, length `m_arms`. Default
#'   `seq_len(m_arms)`.
#' @param Y Optional outcome function of `Z`, `u`, and `outcome_sds`. The
#'   library default is used when `Y` is `NULL`.
#' @return A design object.
#' @seealso [make_design()]
#' @export
#' @examples
#' \dontrun{
#' make_design("multiarm_trial", N = 90)
#' multi_arm_designer(m_arms = 4)
#' multi_arm_designer(m_arms = 4, outcome_means = c(0, 0.5, 1, 2))
#' }
multi_arm_designer <- function(
  N = 90,
  m_arms = 3,
  outcome_means = rep(0, m_arms),
  sd_i = 1,
  outcome_sds = rep(0, m_arms),
  conditions = seq_len(m_arms),
  Y = NULL,
  args_to_fix = NULL
) {
  warn_args_to_fix(args_to_fix)
  m <- as.integer(m_arms)[[1]]
  if (is.na(m) || m < 2L) {
    stop("m_arms must be an integer >= 2.", call. = FALSE)
  }
  om <- as.numeric(outcome_means)
  if (length(om) == 1L) {
    om <- rep(om, m)
  } else if (length(om) != m) {
    stop("outcome_means must have length m_arms.", call. = FALSE)
  }
  os <- as.numeric(outcome_sds)
  if (length(os) == 1L) {
    os <- rep(os, m)
  } else if (length(os) != m) {
    stop("outcome_sds must have length m_arms.", call. = FALSE)
  }
  cond <- conditions
  if (length(cond) != m) {
    stop("conditions must have length m_arms.", call. = FALSE)
  }
  dots <- list(
    N = N,
    m_arms = m,
    outcome_means = om,
    outcome_sds = os,
    conditions = cond,
    sd_i = sd_i
  )
  if (!is.null(Y)) {
    if (!is.function(Y)) {
      stop("Y must be a function.", call. = FALSE)
    }
    dots$Y <- Y
  }
  call_library_design("multiarm_trial", dots)
}

#' Create a two-by-two factorial design
#'
#' Routes to [make_design()] with id `"two_by_two"`:
#' `make_design("two_by_two", N = N, outcome_means = outcome_means, ...)`.
#'
#' Cell means are `outcome_means` in order AB = 00, 01, 10, 11. Scalar
#' `mean_A0B0` / `mean_A0B1` / `mean_A1B0` / `mean_A1B1` override those
#' entries when supplied.
#'
#' @inheritParams two_arm_designer
#' @param prob_A Probability of assignment to A = 1.
#' @param prob_B Probability of assignment to B = 1.
#' @param weight_A Weight on A = 1 when defining the average effect of B.
#' @param weight_B Weight on B = 1 when defining the average effect of A.
#' @param outcome_means Average outcome in each cell, length 4.
#' @param mean_A0B0,mean_A0B1,mean_A1B0,mean_A1B1 Optional cell-mean overrides.
#' @param sd_i Standard deviation of the individual-level shock.
#' @param outcome_sds Extra standard deviation in each cell, length 4.
#' @return A design object.
#' @seealso [make_design()]
#' @export
two_by_two_designer <- function(
  N = 100,
  prob_A = 0.5,
  prob_B = 0.5,
  weight_A = 0.5,
  weight_B = 0.5,
  outcome_means = c(0, 0, 0, 0),
  mean_A0B0 = NULL,
  mean_A0B1 = NULL,
  mean_A1B0 = NULL,
  mean_A1B1 = NULL,
  sd_i = 1,
  outcome_sds = c(0, 0, 0, 0),
  args_to_fix = NULL
) {
  warn_args_to_fix(args_to_fix)
  om <- as.numeric(outcome_means)
  if (length(om) != 4L) {
    stop("outcome_means must have length 4 (AB = 00, 01, 10, 11).", call. = FALSE)
  }
  if (!is.null(mean_A0B0)) om[[1]] <- mean_A0B0
  if (!is.null(mean_A0B1)) om[[2]] <- mean_A0B1
  if (!is.null(mean_A1B0)) om[[3]] <- mean_A1B0
  if (!is.null(mean_A1B1)) om[[4]] <- mean_A1B1
  os <- as.numeric(outcome_sds)
  if (length(os) != 4L) {
    stop("outcome_sds must have length 4.", call. = FALSE)
  }
  call_library_design("two_by_two", list(
    N = N,
    prob_A = prob_A,
    prob_B = prob_B,
    weight_A = weight_A,
    weight_B = weight_B,
    outcome_means = om,
    sd_i = sd_i,
    outcome_sds = os
  ))
}

#' Create a blocked and clustered two-arm design
#'
#' Routes to [make_design()] with id `"block_cluster_two_arm"`:
#' `make_design("block_cluster_two_arm", N_blocks = N_blocks, ...)`.
#'
#' A simplified signature relative to DesignLibrary: level sizes and three
#' shock standard deviations, a scalar assignment probability, and `ate`.
#' Extra DesignLibrary arguments (`N`, `sd`, `rho`, `verbose`, per-block
#' `assignment_probs`) are accepted in `...` and ignored with a warning.
#'
#' @param N_blocks Number of blocks.
#' @param N_clusters_in_block Number of clusters in each block.
#' @param N_i_in_cluster Number of units in each cluster.
#' @param sd_block Standard deviation of the block-level shock.
#' @param sd_cluster Standard deviation of the cluster-level shock.
#' @param sd_i,sd_i_0 Individual-level shock. `sd_i_0` is an alias.
#' @param ate Average treatment effect.
#' @param assignment_prob,assignment_probs Assignment probability. A vector
#'   of per-block probabilities is not supported.
#' @param args_to_fix Ignored.
#' @param ... Unused DesignLibrary arguments; warned and dropped.
#' @return A design object.
#' @seealso [make_design()]
#' @export
block_cluster_two_arm_designer <- function(
  N_blocks = 20,
  N_clusters_in_block = 4,
  N_i_in_cluster = 10,
  sd_block = 0.577,
  sd_cluster = 0.577,
  sd_i = 0.577,
  sd_i_0 = NULL,
  ate = 0.2,
  assignment_prob = 0.5,
  assignment_probs = NULL,
  args_to_fix = NULL,
  ...
) {
  warn_args_to_fix(args_to_fix)
  extra <- list(...)
  extra <- extra[nzchar(names(extra) %||% "")]
  if (length(extra)) {
    warning(
      "Ignoring unused arguments: ", paste(names(extra), collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.null(sd_i_0)) sd_i <- sd_i_0
  if (!is.null(assignment_probs)) {
    if (length(assignment_probs) != 1L) {
      stop(
        "This design uses a single assignment_prob; per-block assignment_probs are not supported.",
        call. = FALSE
      )
    }
    assignment_prob <- assignment_probs
  }
  call_library_design("block_cluster_two_arm", list(
    N_blocks = N_blocks,
    N_clusters_in_block = N_clusters_in_block,
    N_i_in_cluster = N_i_in_cluster,
    sd_block = sd_block,
    sd_cluster = sd_cluster,
    sd_i = sd_i,
    ate = ate,
    assignment_prob = assignment_prob
  ))
}

#' Stop with related make_design() calls for a DesignLibrary designer we did not port
#'
#' These names used to message and return `invisible(NULL)`, which reads as a
#' courtesy in a new package and as a fault in a new major version of an old
#' one: `design <- factorial_designer(k = 3)` then failed several lines later,
#' inside `diagnose_design()`, with an error naming neither the designer nor
#' the replacement. Stopping puts the complaint at the call that caused it.
#'
#' @noRd
designer_not_ported <- function(old, suggestions) {
  stop(
    old, "() is not in ResearchDesigns. Related designs include:\n",
    paste0("  ", suggestions, collapse = "\n"),
    call. = FALSE
  )
}

#' DesignLibrary designers not ported as-is
#'
#' These names exist so that code written for DesignLibrary fails with an
#' error that says what to write instead, rather than with "object not found"
#' or, worse, several lines later on a `NULL` design. Each names the related
#' declarations available through [make_design()], for example
#' `make_design("encouragement")` or `make_design("factorial_2x2")`.
#'
#' @name designers-not-ported
#' @param ... Ignored.
#' @return Nothing: these always stop.
#' @seealso [make_design()]
#' @keywords internal
NULL

#' @rdname designers-not-ported
#' @export
binary_iv_designer <- function(...) {
  designer_not_ported(
    "binary_iv_designer",
    c(
      'make_design("encouragement")',
      'make_design("instrumental_variables")'
    )
  )
}

#' @rdname designers-not-ported
#' @export
cluster_sampling_designer <- function(...) {
  designer_not_ported(
    "cluster_sampling_designer",
    'make_design("cluster_random_sampling")'
  )
}

#' @rdname designers-not-ported
#' @export
factorial_designer <- function(...) {
  designer_not_ported(
    "factorial_designer",
    c(
      'make_design("factorial_2x2")',
      'make_design("factorial_2x2x2")',
      "two_by_two_designer()"
    )
  )
}

#' @rdname designers-not-ported
#' @export
process_tracing_designer <- function(...) {
  designer_not_ported(
    "process_tracing_designer",
    'make_design("process_tracing")'
  )
}

#' @rdname designers-not-ported
#' @export
regression_discontinuity_designer <- function(...) {
  designer_not_ported(
    "regression_discontinuity_designer",
    c(
      'make_design("regression_discontinuity")',
      'make_design("regression_discontinuity_fuzzy")'
    )
  )
}

#' @rdname designers-not-ported
#' @export
spillover_designer <- function(...) {
  designer_not_ported(
    "spillover_designer",
    'make_design("randomized_saturation")'
  )
}

#' @rdname designers-not-ported
#' @export
two_arm_covariate_designer <- function(...) {
  designer_not_ported(
    "two_arm_covariate_designer",
    'make_design("covariate_adjustment")'
  )
}
