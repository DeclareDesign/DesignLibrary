---
id: subgroup_effects
alias: "18.6"
label: subgroup effects
category: rdss
keywords: [experiment, causal]
description: >
  Subgroup design declaration. (chapter 18).
params:
  "n_x1": "Sample size in subgroup X1 (or size allocated to X1)"
  "total_n": "Total sample size"
  "dataset": "Fixed population; pass via make_design(..., dataset = ...). Not edited in the browser."
book_link: https://book.declaredesign.org/library/experimental-causal.html#def-ch18num6
include_in_shiny: true
---

set.seed(343)

fixed_pop <-
  fabricate(N = 10000,
            X = rbinom(N, 1, 0.2),
            potential_outcomes(
              Y ~ rbinom(N, 1,
                         prob = 0.7 + 0.1 * Z  - 0.4 * X - 0.2 * Z * X))
  )
dataset <- fixed_pop

# Note: n_x2 = total_n - n_x1

design <-
  declare_parameters(
    total_n = 1000,
    n_x1 = 500
  ) +
  declare_model(data = dataset,
                TE = (Y_Z_1 - Y_Z_0) ) +  # Individual level effects
  declare_inquiry(
    # Difference in conditional average effects 
    diff_in_CATEs = mean(TE[X == 1])  - mean(TE[X == 0])
  ) +
  declare_sampling(
    S = strata_rs(strata = X, strata_n = c(total_n - n_x1, n_x1))
  ) +
  declare_assignment(Z = block_ra(blocks = X)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z + X + Z * X, 
                    term = "Z:X", 
                    inquiry = "diff_in_CATEs")
