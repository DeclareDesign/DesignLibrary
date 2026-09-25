# `ate` feeds treatment means computed in the body, before any step reads it.
test_designer_contract("block_cluster_two_arm_designer", redesign_exempt = c("ate"))

test_that("block_cluster_two_arm_designer refuses invalid arguments", {
  expect_error(block_cluster_two_arm_designer(N_blocks = -2), "N_* arguments must be positive integers", fixed = TRUE)
  expect_error(block_cluster_two_arm_designer(sd_block = -1), "sd_block must be nonnegative", fixed = TRUE)
  expect_error(block_cluster_two_arm_designer(sd_cluster = -1), "sd_cluster must be nonnegative", fixed = TRUE)
  expect_error(block_cluster_two_arm_designer(sd_i_0 = -1), "sd_i_0 must be nonnegative", fixed = TRUE)
  expect_error(block_cluster_two_arm_designer(sd_i_1 = -1), "sd_i_1 must be nonnegative", fixed = TRUE)
  expect_error(block_cluster_two_arm_designer(assignment_probs = 10), "all assignment_probs must be in (0,1)", fixed = TRUE)
  expect_error(block_cluster_two_arm_designer(assignment_probs = 2:10 / (sum(2:10))), "assignment_probs must either be a scalar, or of length N_blocks.", fixed = TRUE)
  expect_error(block_cluster_two_arm_designer(rho = 10), "correlation must be in [-1,1]", fixed = TRUE)
  expect_error(block_cluster_two_arm_designer(N = 1, N_i_in_cluster = 10), "The design N of 10 is inconsistent with the user specified N of 1.", fixed = TRUE)
  expect_error(block_cluster_two_arm_designer(N_blocks = 2, N_clusters_in_block = 2, N_i_in_cluster = c(4, 6)), "N_i_in_cluster should be of length 1 or of length 4", fixed = TRUE)
  expect_error(block_cluster_two_arm_designer(N_blocks = 2, N_clusters_in_block = c(2, 2), N_i_in_cluster = c(4, 6)), "N_i_in_cluster should be of length 1 or of length 4", fixed = TRUE)
  expect_error(block_cluster_two_arm_designer(N_blocks = 4, N_clusters_in_block = c(2, 2)), "You specified a design with 4 blocks, but specified N_clusters_in_block for 2 blocks.", fixed = TRUE)
})

test_that("block_cluster_two_arm_designer reports the implied ICC only when verbose", {
  expect_output(block_cluster_two_arm_designer(sd = 2), "implied ICC")
  expect_silent(block_cluster_two_arm_designer(sd = 2, verbose = FALSE))
  expect_output(block_cluster_two_arm_designer(sd = 1, sd_block = 2, verbose = TRUE), "implied ICC")
  expect_silent(block_cluster_two_arm_designer(sd = 1, sd_block = 2, verbose = FALSE))
})

test_that("block_cluster_two_arm_designer asks for CR2 standard errors, as 0.1.10 got by default", {
  design <- block_cluster_two_arm_designer(verbose = FALSE)
  dat <- draw_data(design)
  est <- get_estimates(design, data = dat)
  cr2 <- estimatr::lm_robust(Y ~ Z, data = dat, fixed_effects = ~ blocks,
                             clusters = clusters, se_type = "CR2")
  expect_equal(est$std.error, unname(cr2$std.error["Z"]))
})

test_that("block_cluster_two_arm_designer recycles assignment_probs over blocks", {
  design <- redesign(block_cluster_two_arm_designer(verbose = FALSE), N_blocks = 4)
  expect_no_error(draw_data(design))
})
