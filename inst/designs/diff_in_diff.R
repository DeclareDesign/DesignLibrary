---
id: diff_in_diff
alias: "16.3"
label: diff-in-diff
category: rdss
keywords: [observational, causal]
description: >
  Difference-in-differences design (chapter 16).
packages: [rdss, DIDmultiplegt, dplyr]
params:
  "N_time_periods": "Number of time periods"
  "N_units": "Number of units (panel cross-section size)"
book_link: https://book.declaredesign.org/library/observational-causal.html#def-ch16num3
include_in_shiny: true
---

design <- 
  declare_parameters(
    N_units = 20,
    N_time_periods = 20
  ) +
  declare_model(
  units = declare_level(
    N = N_units, 
    U_unit = rnorm(N), 
    D_unit = if_else(U_unit > median(U_unit), 1, 0),
    D_time = sample(1:N_time_periods, N, replace = TRUE)
  ),
  periods = declare_level(
    N = N_time_periods,
    U_time = rnorm(N)
  ),
  unit_period = cross_levels(
    .by = c("units", "periods"), 
    U = rnorm(N),
    potential_outcomes(Y ~ U + U_unit + U_time + 
                         D * (0.2 - 1 * (D_time - as.numeric(periods))), 
                       conditions = list(D = c(0, 1))),
    D = if_else(D_unit == 1 & as.numeric(periods) >= D_time, 1, 0),
    D_lag = lag_by_group(D, groups = units, n = 1, order_by = periods)
  )
) +
  declare_inquiry(
    ATT = mean(Y_D_1 - Y_D_0), 
    subset = D == 1
  ) + 
  declare_inquiry(
    ATT_switchers = mean(Y_D_1 - Y_D_0), 
    subset = D == 1 & D_lag == 0 & !is.na(D_lag)
  ) +
  declare_measurement(Y = reveal_outcomes(Y ~ D)) +
  declare_estimator(
    Y ~ D, fixed_effects = ~ units + periods,
    .method = lm_robust,
    inquiry = c("ATT", "ATT_switchers"),
    label = "twoway-fe"
  ) +
  # KNOWN ISSUE, upstream and not ours: under DIDmultiplegt 2.1.0 this estimator
  # returns NaN on this design's data. The data is a clean staggered adoption
  # panel (10 never-treated units, 10 adopting at different periods, absorbing),
  # which is what did_multiplegt is for. Checked outside DeclareDesign entirely,
  # and via did_multiplegt_old, did_multiplegt(mode = "old") and mode passed
  # trailing: all three give NaN, so it is not the argument convention and not
  # the quoted-name workaround. tools/check_designs_isolated.R flags it.
  declare_estimator(
    Y = "Y", 
    G = "units", 
    T = "periods", 
    D = "D",
    mode = "old",
    handler = label_estimator(did_multiplegt_tidy),
    inquiry = c("ATT", "ATT_switchers"),
    label = "chaisemartin"
  ) 
