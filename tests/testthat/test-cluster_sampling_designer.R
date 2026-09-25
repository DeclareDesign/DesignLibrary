# `icc` sets the population, which is drawn once when the design is built.
test_designer_contract("cluster_sampling_designer", redesign_exempt = c("icc"))

test_that("cluster_sampling_designer refuses invalid arguments", {
  expect_error(cluster_sampling_designer(n_clusters_in_block = 10, N_clusters_in_block = 1), "n_clusters_in_block sampled must be smaller than the total number of 1 clusters.", fixed = TRUE)
  expect_error(cluster_sampling_designer(n_i_in_cluster = 30, N_i_in_cluster = 10), "n_i_in_cluster must be smaller than or equal to the minimum of 10 subjects per cluster.", fixed = TRUE)
  expect_error(cluster_sampling_designer(icc = 2), "icc must be a number in [0,1]", fixed = TRUE)
})
