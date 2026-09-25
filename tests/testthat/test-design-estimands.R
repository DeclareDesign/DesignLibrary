test_that("audit_experiment's outcomes reveal its estimand", {
  skip_if_not_installed("DeclareDesign")

  dat <- DeclareDesign::draw_data(make_design("audit_experiment"))
  expect_setequal(unique(dat$type), c("Always-responder", "Anti-Latino discriminator", "Never-responder"))
  # Only discriminators respond to the white name and not the Latino one
  expect_equal(mean(dat$Y_Z_white - dat$Y_Z_latino), mean(dat$type == "Anti-Latino discriminator"))
})

test_that("regression_discontinuity's estimand is the jump at the estimator's cutoff", {
  skip_if_not_installed("DeclareDesign")
  skip_if_not_installed("rdss")
  skip_if_not_installed("rdrobust")

  d <- make_design("regression_discontinuity")
  dat <- DeclareDesign::draw_data(d)
  estimand <- DeclareDesign::draw_estimands(d)$estimand
  # The running variable is centered, and the estimator is called with c = 0
  near_cutoff <- abs(dat$X) < 0.01
  expect_gt(sum(near_cutoff), 0)
  expect_equal(mean(dat$Y_D_1[near_cutoff] - dat$Y_D_0[near_cutoff]), estimand, tolerance = 0.02)
})
