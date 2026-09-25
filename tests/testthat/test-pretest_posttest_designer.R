test_designer_contract("pretest_posttest_designer")

test_that("pretest_posttest_designer refuses invalid arguments", {
  expect_error(pretest_posttest_designer(rho = 10), "'rho' must be a value in [-1, 1]", fixed = TRUE)
  expect_error(pretest_posttest_designer(attrition_rate = 10), "'attrition_rate' must be in [0,1]", fixed = TRUE)
  expect_error(pretest_posttest_designer(sd_1 = -1), "'sd_1' and 'sd_2' must be nonnegative", fixed = TRUE)
})
