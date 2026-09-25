test_designer_contract("spillover_designer")

test_that("spillover_designer refuses invalid arguments", {
  expect_error(spillover_designer(sd_i = -10), "sd_i must be nonnegative", fixed = TRUE)
  expect_error(spillover_designer(N_i_group = -10), "N_i_group and N_groups must be equal to or greater than 1", fixed = TRUE)
})

test_that("simple_spillover_designer is deprecated in favour of spillover_designer", {
  expect_warning(simple_spillover_designer(), "spillover_designer", fixed = TRUE)
})
