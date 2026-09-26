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
#' Argument names match DesignLibrary 0.1's `two_arm_designer`.
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
#' @param args_to_fix Ignored. Present for compatibility with DesignLibrary 0.1.
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
#' A simplified signature relative to DesignLibrary 0.1: level sizes and three
#' shock standard deviations, a scalar assignment probability, and `ate`.
#' Extra DesignLibrary 0.1 arguments (`N`, `sd`, `rho`, `verbose`, per-block
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
#' @param ... Unused DesignLibrary 0.1 arguments; warned and dropped.
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

#' Create a two-arm design with a possibly prognostic covariate
#'
#' Routes to [make_design()] with id `"two_arm_covariate"`:
#' `make_design("two_arm_covariate", N = N, rho_WY = rho_WY, ...)`.
#'
#' The covariate `W` can predict the outcome (`rho_WY`), moderate the effect
#' (`h`), and predict assignment (`rho_WZ`). With `rho_WZ = 0` assignment is
#' random and adjustment buys precision; otherwise `W` confounds. Argument
#' names match DesignLibrary 0.1's `two_arm_covariate_designer`.
#'
#' @inheritParams two_arm_designer
#' @param prob Probability of assignment to treatment.
#' @param sd Standard deviation of the outcome shock.
#' @param h Heterogeneity of the treatment effect by `W`.
#' @param treatment_mean Average outcome in treatment. If supplied, overrides
#'   `ate` (`ate` becomes `treatment_mean - control_mean`).
#' @param rho_WY Correlation between `W` and the outcome shock (-1 to 1).
#' @param rho_WZ Correlation between `W` and the latent assignment variable
#'   (-1 to 1).
#' @return A design object.
#' @seealso [make_design()]
#' @export
#' @examples
#' \dontrun{
#' prognostic <- two_arm_covariate_designer(N = 40, ate = 0.2, rho_WY = 0.9, h = 0.5)
#' confounded <- two_arm_covariate_designer(N = 40, ate = 0, rho_WZ = 0.9, rho_WY = 0.9)
#' }
two_arm_covariate_designer <- function(
  N = 100,
  prob = 0.5,
  control_mean = 0,
  sd = 1,
  ate = 1,
  h = 0,
  treatment_mean = NULL,
  rho_WY = 0,
  rho_WZ = 0,
  args_to_fix = NULL
) {
  warn_args_to_fix(args_to_fix)
  if (sd < 0) stop("sd must be non-negative.", call. = FALSE)
  if (prob < 0 || prob > 1) stop("prob must be in [0, 1].", call. = FALSE)
  if (abs(rho_WY) > 1) stop("rho_WY must be in [-1, 1].", call. = FALSE)
  if (abs(rho_WZ) > 1) stop("rho_WZ must be in [-1, 1].", call. = FALSE)
  if (!is.null(treatment_mean)) ate <- treatment_mean - control_mean
  call_library_design("two_arm_covariate", list(
    N = N,
    prob = prob,
    control_mean = control_mean,
    sd = sd,
    ate = ate,
    h = h,
    rho_WY = rho_WY,
    rho_WZ = rho_WZ
  ))
}

#' Create a regression discontinuity design with a polynomial fit
#'
#' Routes to [make_design()] with id `"regression_discontinuity_polynomial"`:
#' `make_design("regression_discontinuity_polynomial", N = N, tau = tau, ...)`.
#'
#' Treatment is assigned when a uniform running variable passes `cutoff`, and
#' each potential outcome is a polynomial in the running variable. Argument
#' names match DesignLibrary 0.1's `regression_discontinuity_designer`. For the
#' book's local-linear version, see `make_design("regression_discontinuity")`.
#'
#' @inheritParams two_arm_designer
#' @param tau Jump in the outcome at the cutoff.
#' @param outcome_sd Standard deviation of the outcome shock.
#' @param cutoff Cutoff on the running variable, in (0, 1).
#' @param bandwidth Largest distance from the cutoff for a unit to be kept.
#' @param control_coefs Polynomial coefficients of the control outcome in the
#'   running variable.
#' @param treatment_coefs Polynomial coefficients of the treated outcome in the
#'   running variable.
#' @param poly_reg_order Order of the polynomial in the estimator.
#' @return A design object.
#' @seealso [make_design()]
#' @export
#' @examples
#' \dontrun{
#' regression_discontinuity_designer(N = 500, poly_reg_order = 2)
#' }
regression_discontinuity_designer <- function(
  N = 1000,
  tau = 0.15,
  outcome_sd = 0.1,
  cutoff = 0.5,
  bandwidth = 0.5,
  control_coefs = c(0.5, 0.5),
  treatment_coefs = c(-5, 1),
  poly_reg_order = 4,
  args_to_fix = NULL
) {
  warn_args_to_fix(args_to_fix)
  if (cutoff <= 0 || cutoff >= 1) stop("cutoff must be in (0, 1).", call. = FALSE)
  if (poly_reg_order < 1 || poly_reg_order %% 1 != 0) {
    stop("poly_reg_order must be an integer of at least 1.", call. = FALSE)
  }
  if (length(control_coefs) < 1) stop("control_coefs must have length > 0.", call. = FALSE)
  if (length(treatment_coefs) < 1) stop("treatment_coefs must have length > 0.", call. = FALSE)
  if (outcome_sd < 0) stop("outcome_sd must be non-negative.", call. = FALSE)
  call_library_design("regression_discontinuity_polynomial", list(
    N = N,
    tau = tau,
    outcome_sd = outcome_sd,
    cutoff = cutoff,
    bandwidth = bandwidth,
    control_coefs = control_coefs,
    treatment_coefs = treatment_coefs,
    poly_reg_order = poly_reg_order
  ))
}

#' Create a two-arm design with spillovers within groups
#'
#' Routes to [make_design()] with id `"spillover"`:
#' `make_design("spillover", N_groups = N_groups, gamma = gamma, ...)`.
#'
#' A unit's outcome depends on the share of its group that is treated, raised
#' to the power `gamma`. Argument names match DesignLibrary 0.1's
#' `spillover_designer`. For a design that randomizes saturation, see
#' `make_design("randomized_saturation")`.
#'
#' @inheritParams two_arm_designer
#' @param N_groups Number of groups.
#' @param N_i_group Number of units in each group.
#' @param sd_i Standard deviation of the individual-level shock.
#' @param gamma Curvature of the outcome in the group's treated share.
#' @return A design object.
#' @seealso [make_design()]
#' @export
#' @examples
#' \dontrun{
#' spillover_designer(N_groups = 40, gamma = 1)
#' }
spillover_designer <- function(
  N_groups = 80,
  N_i_group = 3,
  sd_i = 0.2,
  gamma = 2,
  args_to_fix = NULL
) {
  warn_args_to_fix(args_to_fix)
  if (sd_i < 0) stop("sd_i must be non-negative.", call. = FALSE)
  if (N_i_group < 1 || N_groups < 1) {
    stop("N_i_group and N_groups must be at least 1.", call. = FALSE)
  }
  call_library_design("spillover", list(
    N_groups = N_groups, N_i_group = N_i_group, sd_i = sd_i, gamma = gamma
  ))
}

#' Create a two-stage cluster sampling design
#'
#' Routes to [make_design()] with id `"cluster_sampling"`:
#' `make_design("cluster_sampling", n_clusters_in_block = n_clusters_in_block, ...)`.
#'
#' Clusters are sampled within blocks, then individuals within sampled
#' clusters, from a population drawn once when the design is built. Argument
#' names match DesignLibrary 0.1's `cluster_sampling_designer`. For the book's
#' budget-constrained version, see `make_design("cluster_random_sampling")`.
#'
#' @inheritParams two_arm_designer
#' @param N_blocks Number of blocks.
#' @param N_clusters_in_block Number of clusters in each block.
#' @param N_i_in_cluster Number of individuals in each cluster.
#' @param n_clusters_in_block Number of clusters sampled in each block.
#' @param n_i_in_cluster Number of individuals sampled in each sampled cluster.
#' @param icc Intra-cluster correlation of the latent outcome, in \[0, 1\].
#' @return A design object.
#' @seealso [make_design()]
#' @export
#' @examples
#' \dontrun{
#' cluster_sampling_designer(N_clusters_in_block = 200, n_clusters_in_block = 20)
#' }
cluster_sampling_designer <- function(
  N_blocks = 1,
  N_clusters_in_block = 1000,
  N_i_in_cluster = 50,
  n_clusters_in_block = 100,
  n_i_in_cluster = 10,
  icc = 0.2,
  args_to_fix = NULL
) {
  warn_args_to_fix(args_to_fix)
  if (n_clusters_in_block > min(N_clusters_in_block)) {
    stop("n_clusters_in_block must not exceed N_clusters_in_block.", call. = FALSE)
  }
  if (n_i_in_cluster > min(N_i_in_cluster)) {
    stop("n_i_in_cluster must not exceed N_i_in_cluster.", call. = FALSE)
  }
  if (icc < 0 || icc > 1) stop("icc must be in [0, 1].", call. = FALSE)
  call_library_design("cluster_sampling", list(
    N_blocks = N_blocks,
    N_clusters_in_block = N_clusters_in_block,
    N_i_in_cluster = N_i_in_cluster,
    n_clusters_in_block = n_clusters_in_block,
    n_i_in_cluster = n_i_in_cluster,
    icc = icc
  ))
}

#' Create a binary instrumental variables design
#'
#' Routes to [make_design()] with id `"binary_iv"`:
#' `make_design("binary_iv", N = N, type_probs = type_probs, ...)`.
#'
#' Units are always-takers, never-takers, compliers, or defiers, in that order
#' in every length-4 argument. `a_Y`, `b_Y`, and `d_Y` fill the defaults of
#' `a`, `b`, and `d` and are not passed on. Argument names match DesignLibrary
#' 0.1's `binary_iv_designer`. See also `make_design("encouragement")` and
#' `make_design("instrumental_variables")`.
#'
#' @inheritParams two_arm_designer
#' @param type_probs Shares of always-takers, never-takers, compliers, and
#'   defiers.
#' @param assignment_probs Probability that Z = 1 for each type.
#' @param a_Y Outcome intercept for always-takers, used in the default `a`.
#' @param b_Y Effect of X on the outcome for every type, used in the default
#'   `b`.
#' @param d_Y Direct effect of Z on the outcome for every type, used in the
#'   default `d`.
#' @param outcome_sd Standard deviation of the outcome shock.
#' @param a,b,d Length-4 outcome intercepts, effects of X, and direct effects
#'   of Z, by type.
#' @return A design object.
#' @seealso [make_design()]
#' @export
#' @examples
#' \dontrun{
#' binary_iv_designer(N = 200, b_Y = 0.5)
#' binary_iv_designer(type_probs = c(0.2, 0.2, 0.5, 0.1), b = c(0, 0, 1, -1))
#' }
binary_iv_designer <- function(
  N = 100,
  type_probs = c(1/3, 1/3, 1/3, 0),
  assignment_probs = c(0.5, 0.5, 0.5, 0.5),
  a_Y = 1,
  b_Y = 0,
  d_Y = 0,
  outcome_sd = 1,
  a = c(1, 0, 0, 0) * a_Y,
  b = rep(b_Y, 4),
  d = rep(d_Y, 4),
  args_to_fix = NULL
) {
  warn_args_to_fix(args_to_fix)
  if (min(assignment_probs) < 0) stop("assignment_probs must be non-negative.", call. = FALSE)
  if (max(assignment_probs) > 1) stop("assignment_probs must be at most 1.", call. = FALSE)
  if (outcome_sd < 0) stop("outcome_sd must be non-negative.", call. = FALSE)
  if (length(a) != 4) stop("a must have length 4.", call. = FALSE)
  if (length(b) != 4) stop("b must have length 4.", call. = FALSE)
  if (length(d) != 4) stop("d must have length 4.", call. = FALSE)
  call_library_design("binary_iv", list(
    N = N,
    type_probs = type_probs,
    assignment_probs = assignment_probs,
    outcome_sd = outcome_sd,
    a = a,
    b = b,
    d = d
  ))
}

#' Create a process tracing design with two clues
#'
#' Routes to [make_design()] with id `"process_tracing_bayes"`:
#' `make_design("process_tracing_bayes", prior_H = prior_H, ...)`.
#'
#' One case with X = 1 and Y = 1 is selected, two clues are observed, and the
#' posterior that X caused Y is updated by Bayes' rule. Argument names match
#' DesignLibrary 0.1's `process_tracing_designer`. The estimator labels are
#' fixed at "Straw in the Wind" and "Smoking Gun", so `label_E1` and
#' `label_E2` other than those warn and are ignored. For the book's
#' CausalQueries version, see `make_design("process_tracing")`.
#'
#' @inheritParams two_arm_designer
#' @param N Number of cases in the population.
#' @param prob_X Probability that X = 1.
#' @param process_proportions Shares of the four causal processes: X causes
#'   Y, Y regardless, X causes not Y, and not Y regardless.
#' @param prior_H Prior probability that X caused Y.
#' @param p_E1_H,p_E1_not_H Probability of clue 1 if X did and did not cause Y.
#' @param p_E2_H,p_E2_not_H Probability of clue 2 if X did and did not cause Y.
#' @param cor_E1E2_H,cor_E1E2_not_H Correlation of the two clues if X did and
#'   did not cause Y.
#' @param label_E1,label_E2 Ignored unless left at their defaults.
#' @return A design object.
#' @seealso [make_design()]
#' @export
#' @examples
#' \dontrun{
#' process_tracing_designer(prior_H = 0.3, p_E2_H = 0.5)
#' }
process_tracing_designer <- function(
  N = 100,
  prob_X = 0.5,
  process_proportions = c(0.25, 0.25, 0.25, 0.25),
  prior_H = 0.5,
  p_E1_H = 0.8,
  p_E1_not_H = 0.2,
  p_E2_H = 0.3,
  p_E2_not_H = 0,
  cor_E1E2_H = 0,
  cor_E1E2_not_H = 0,
  label_E1 = "Straw in the Wind",
  label_E2 = "Smoking Gun",
  args_to_fix = NULL
) {
  warn_args_to_fix(args_to_fix)
  if (N < 1 || N %% 1 != 0) stop("N must be a positive integer.", call. = FALSE)
  if (length(process_proportions) != 4) stop("process_proportions must have length 4.", call. = FALSE)
  if (abs(sum(process_proportions) - 1) > 1e-8 || any(process_proportions < 0)) {
    stop("process_proportions must be non-negative and sum to 1.", call. = FALSE)
  }
  probs <- c(prob_X = prob_X, prior_H = prior_H, p_E1_H = p_E1_H,
             p_E1_not_H = p_E1_not_H, p_E2_H = p_E2_H, p_E2_not_H = p_E2_not_H)
  bad <- names(probs)[probs < 0 | probs > 1]
  if (length(bad)) stop(bad[[1]], " must be in [0, 1].", call. = FALSE)
  if (abs(cor_E1E2_H) > 1) stop("cor_E1E2_H must be in [-1, 1].", call. = FALSE)
  if (abs(cor_E1E2_not_H) > 1) stop("cor_E1E2_not_H must be in [-1, 1].", call. = FALSE)
  clue_pair_min <- function(p1, p2, rho) {
    r <- rho * (p1 * p2 * (1 - p1) * (1 - p2))^0.5
    min((1 - p1) * (1 - p2) + r, p2 * (1 - p1) - r, p1 * (1 - p2) - r, p1 * p2 + r)
  }
  if (clue_pair_min(p_E1_H, p_E2_H, cor_E1E2_H) < 0 ||
      clue_pair_min(p_E1_not_H, p_E2_not_H, cor_E1E2_not_H) < 0) {
    stop("Correlation coefficient not compatible with probabilities.", call. = FALSE)
  }
  if (!identical(label_E1, "Straw in the Wind") || !identical(label_E2, "Smoking Gun")) {
    warning("label_E1 and label_E2 are ignored; the estimator labels are fixed.", call. = FALSE)
  }
  call_library_design("process_tracing_bayes", list(
    N = N,
    prob_X = prob_X,
    process_proportions = process_proportions,
    prior_H = prior_H,
    p_E1_H = p_E1_H,
    p_E1_not_H = p_E1_not_H,
    p_E2_H = p_E2_H,
    p_E2_not_H = p_E2_not_H,
    cor_E1E2_H = cor_E1E2_H,
    cor_E1E2_not_H = cor_E1E2_not_H
  ))
}

#' Stop with related make_design() calls for a DesignLibrary 0.1 designer not yet ported
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
    old, "() from DesignLibrary 0.1 is not in DesignLibrary 2.0 yet. Related designs include:\n",
    paste0("  ", suggestions, collapse = "\n"),
    call. = FALSE
  )
}

#' DesignLibrary 0.1 designers not ported as-is
#'
#' These names exist so that code written for DesignLibrary 0.1 fails with an
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



