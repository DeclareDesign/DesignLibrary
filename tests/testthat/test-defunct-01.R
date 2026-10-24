# Removed DesignLibrary 0.1 exports ----

test_that("0.1 designer aliases error and name their replacement", {
  replacements <- c(
    simple_two_arm_designer = "two_arm_designer",
    simple_factorial_designer = "two_by_two_designer",
    simple_iv_designer = "binary_iv_designer",
    simple_spillover_designer = "spillover_designer"
  )
  exported <- getNamespaceExports("DesignLibrary")
  for (nm in names(replacements)) {
    expect_true(nm %in% exported, info = nm)
    fn <- get(nm, envir = asNamespace("DesignLibrary"))
    expect_error(fn(N = 10), "removed in DesignLibrary 2.0.0", info = nm)
    expect_error(fn(N = 10), paste0("`", replacements[[nm]], "()`"), fixed = TRUE, info = nm)
  }
})

test_that("get_design_code errors and points to get_code", {
  design <- two_arm_designer(N = 10)
  expect_error(get_design_code(design), "removed in DesignLibrary 2.0.0")
  expect_error(get_design_code(design), "`get_code()`", fixed = TRUE)
})

test_that("match.call.defaults errors and points to match.call", {
  expect_error(match.call.defaults(), "removed in DesignLibrary 2.0.0")
  expect_error(match.call.defaults(), "`match.call()`", fixed = TRUE)
})

# Removed block_cluster_two_arm_designer arguments ----

test_that("block_cluster_two_arm_designer errors on removed 0.1 arguments", {
  expect_error(
    block_cluster_two_arm_designer(N = 200),
    "no longer takes `N`.*N_blocks"
  )
  expect_error(
    block_cluster_two_arm_designer(sd = 2),
    "no longer takes `sd`.*sd_block"
  )
  expect_error(
    block_cluster_two_arm_designer(sd_i_1 = 2),
    "no longer takes `sd_i_1`.*sd_i"
  )
  expect_error(
    block_cluster_two_arm_designer(rho = 0.5),
    "no longer takes `rho`.*not supported"
  )
  expect_error(
    block_cluster_two_arm_designer(control_mean = 0, treatment_mean = 1),
    "no longer takes `control_mean`, `treatment_mean`.*`ate`"
  )
})

test_that("block_cluster_two_arm_designer warns on other unused arguments", {
  expect_warning(
    design <- block_cluster_two_arm_designer(verbose = FALSE),
    "Ignoring unused arguments: verbose"
  )
  expect_s3_class(design, "design")
})
