---
id: cluster_random_sampling
alias: "15.3"
label: cluster random sampling
category: rdss
keywords: [observational, descriptive, cluster]
description: >
  Cluster random sampling design. (chapter 15).
params:
  "cluster_prob": "Cluster sampling probability"
  "state_mean": "State-level mean of the outcome"
  "ICC": "Intracluster correlation of the outcome"
  "locality_shock": "Locality-level shock"
  "individual_shock": "Individual-level shock"
  "budget_function": "Maps cluster sampling probability to an individual sampling probability"
  "se_type": "Standard error type for the cluster-robust estimator"
coupled:
  ICC: [locality_shock, individual_shock]
book_link: https://book.declaredesign.org/library/observational-descriptive.html#def-ch15num3
include_in_shiny: true
---

budget_function <- 
  function(cluster_prob){
    budget = 20000
    cluster_cost = 20
    individual_cost = 2
    n_clusters = 1000
    n_individuals_per_cluster = 100
    
    total_cluster_cost <-
      cluster_prob * n_clusters * cluster_cost
    
    remaining_funds <- budget - total_cluster_cost
    
    sampleable_individuals <- 
      cluster_prob * n_clusters * n_individuals_per_cluster
    
    individual_prob = 
      (remaining_funds/individual_cost)/sampleable_individuals
    
    pmin(individual_prob, 1)
  }

# `ICC` drives both shocks, so it belongs where the shocks are computed from
# it. Written as three top-level objects it looked like three parameters, and
# redesigning `ICC` moved nothing: the shocks had already been drawn. Declared
# here they are evaluated once when the design is built, in order, so a
# redesign of `ICC` redraws both at the new variance and `state_mean`,
# `cluster_prob` and the shocks themselves all stay reachable.
design <-
  declare_parameters(
    se_type = "stata",
    ICC = 0.4,
    state_mean = c(-0.2, 0.2),
    locality_shock = rnorm(500, state_mean, sqrt(ICC)),
    individual_shock = rnorm(100000, sd = sqrt(1 - ICC)),
    cluster_prob = 0.5
  ) +
  declare_model(
    state = add_level(N = 2, 
                      state_name = c("taraba", "kwara"),
                      state_mean = state_mean),
    locality = add_level(N = 500, locality_shock = locality_shock),
    individual = add_level(N = 100, Y_star = locality_shock + individual_shock
  )) +
  declare_measurement(Y = as.numeric(cut(Y_star, 7))) +
  declare_inquiry(Y_bar = mean(Y)) +
  declare_sampling(
    S_cluster = strata_and_cluster_rs(
      strata = state,
      clusters = locality,
      prob = cluster_prob
    ),
    filter = S_cluster == 1
  ) +
  declare_sampling(
    S_individual = 
      strata_rs(strata = locality, 
                prob = budget_function(cluster_prob)),
    filter = S_individual == 1
  ) +
  declare_estimator(Y ~ 1,
                    clusters = locality,
                    se_type = se_type,
                    inquiry = "Y_bar")
