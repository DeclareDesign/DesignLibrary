---
id: network_experiment
alias: "18.13"
label: network experiment
category: rdss
keywords: [experiment, causal]
description: >
  Experiments over spatial networks design. (chapter 18).
packages: [rdss, spdep, interference, tidyverse]
params:
  "controlled_direct_effect": "Controlled direct effect size"
  "controlled_indirect_effect": "Controlled indirect effect size"
  "total_effect": "Total effect size"
  "adjacency": "Adjacency matrix; pass via make_design(..., adjacency = ...). Not edited in the browser."
  "permutations": "Permutation matrix; pass via make_design(..., permutations = ...). Not edited in the browser."
  "estimator_AS": "Function: Aronow-Samii network exposure estimator (R-only)"
book_link: https://book.declaredesign.org/library/experimental-causal.html#def-ch18num13
# adjacency and permutations are package parameters (kind = data), not Shiny
# controls. The browser edits the three effect sizes only.
include_in_shiny: true
functional: true
---

# This is a complex design with many pre-prepared objects





# Background Data: Adjacency matrix
adjacency <-
  fairfax |>
  as("Spatial") |>
  poly2nb(queen = TRUE) |>
  nb2mat(style = "B", zero.policy = TRUE)

# Here we create a permutation matrix of possible random assignments
# Keep maximum_permutations modest to avoid node stack overflow when running in app
ra_declaration <- declare_ra(N = 238, prob = 0.1)

permutations <-
  ra_declaration |>
  obtain_permutation_matrix(maximum_permutations = 1000) |>
  t()

prob_exposure <- interference::make_exposure_prob(
  potential_tr_vector = permutations,
  adj_matrix = adjacency,
  exposure_map_fn = interference::make_exposure_map_AS,
  exposure_map_fn_add_args = list(hop = 1)
)

estimator_AS <-
  function(data, p_matrix, adj_matrix, obs_prob_exposure) {
    
    obs_exposure <-
      interference::make_exposure_map_AS(
        adj_matrix = adj_matrix,
        tr_vector = data$Z,
        hop = 1
      )
    
    out_AS <-
      interference::estimates(
        obs_exposure = obs_exposure,
        obs_outcome = data$Y,
        obs_prob_exposure = obs_prob_exposure,
        n_var_permutations = 30
      )
    
    tibble(
      term = c(names(out_AS$tau_ht), names(out_AS$tau_h)),
      inquiry = rep(c("total_ATE", "direct_ATE", "indirect_ATE"), 2),
      estimator = rep(c("Horvitz-Thompson", "Hajek"), each = 3),
      estimate = c(out_AS$tau_ht, out_AS$tau_h)
    )
  }

design <-
  declare_parameters(
    controlled_direct_effect = 0.02,  # keeping indirect = 0
    controlled_indirect_effect = 0.01,  # keeping direct = 0
    total_effect = 0.03  # via both direct and indirect
  ) +
  declare_model(
    data = select(as_tibble(fairfax), -geometry),
    Y_0_0 = pnorm(scale(SHAPE_LEN), sd = 3),
    Y_1_0 = Y_0_0 + controlled_direct_effect,
    Y_0_1 = Y_0_0 + controlled_indirect_effect,
    Y_1_1 = Y_0_0 + total_effect
  ) +
  declare_inquiry(
    total_ATE = mean(Y_1_1 - Y_0_0),
    direct_ATE = mean(Y_1_0 - Y_0_0),
    indirect_ATE = mean(Y_0_1 - Y_0_0)
  ) +
  
  declare_assignment(
    Z = conduct_ra(ra_declaration),
    exposure = get_exposure_AS(make_exposure_map_AS(adjacency, Z, hop = 1))) +
  
  declare_measurement(
    Y = case_when(
      exposure == "dir_ind1" ~ Y_1_1,
      exposure == "isol_dir" ~ Y_1_0,
      exposure == "ind1" ~ Y_0_1,
      exposure == "no" ~ Y_0_0
    )
  )  +
  declare_estimator(handler = estimator_AS,
                    p_matrix = permutations, 
                    adj_matrix = adjacency,
                    obs_prob_exposure = prob_exposure)
