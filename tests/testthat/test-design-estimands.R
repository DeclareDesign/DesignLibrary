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

# Getting started designs ----

# draw_data() and draw_estimands() each redraw the model, so seed them alike to read one draw
draw_one <- function(design) {
  set.seed(343)
  dat <- DeclareDesign::draw_data(design)
  set.seed(343)
  list(data = dat, estimands = DeclareDesign::draw_estimands(design))
}

test_that("the constant-effect two-arm designs declare the effect they build in", {
  skip_if_not_installed("DeclareDesign")

  cases <- list(
    list(design = make_design("two_arm_simple", b = 0.37), effect = 0.37),
    list(design = make_design("two_arm_flexible", ate = 0.37, rho = 1), effect = 0.37),
    list(design = make_design("two_arm_block_cluster", ate = 0.37), effect = 0.37),
    list(design = make_design("two_arm_with_blocks", b = 0.37), effect = 0.37),
    list(design = make_design("pretest_posttest", ate = 0.37), effect = 0.37)
  )
  for (case in cases) {
    expect_equal(DeclareDesign::draw_estimands(case$design)$estimand, case$effect)
  }
})

test_that("two_by_two's estimands follow its cell means in the order AB = 00, 01, 10, 11", {
  skip_if_not_installed("DeclareDesign")

  d <- make_design("two_by_two", outcome_means = c(0, 1, 2, 4))
  draw <- draw_one(d)
  dat <- draw$data
  expect_equal(mean(dat$Y_A_0_B_1 - dat$Y_A_0_B_0), 1)
  expect_equal(mean(dat$Y_A_1_B_0 - dat$Y_A_0_B_0), 2)
  estimands <- draw$estimands
  expect_equal(estimands$estimand, c(2.5, 1.5, 1))
})

test_that("factorial_2x2x2's estimands are the averages its coefficients imply", {
  skip_if_not_installed("DeclareDesign")

  e1 <- 0.5; e2 <- 0.2; e3 <- -0.3
  i12 <- 0.4; i13 <- -0.1; i23 <- 0.6; i123 <- 1
  d <- make_design(
    "factorial_2x2x2",
    effect_T1 = e1, effect_T2 = e2, effect_T3 = e3,
    interaction_T1_T2 = i12, interaction_T1_T3 = i13,
    interaction_T2_T3 = i23, interaction_T1_T2_T3 = i123
  )
  draw <- draw_one(d)
  dat <- draw$data
  estimands <- draw$estimands
  expected <- c(
    Overall_average = mean(dat$u) + (e1 + e2 + e3) / 2 + (i12 + i13 + i23) / 4 + i123 / 8,
    TE_T1 = e1 + (i12 + i13) / 2 + i123 / 4,
    TE_T2 = e2 + (i12 + i23) / 2 + i123 / 4,
    TE_T3 = e3 + (i13 + i23) / 2 + i123 / 4,
    TE_T1_T2 = i12 + i123 / 2,
    TE_T1_T3 = i13 + i123 / 2,
    TE_T2_T3 = i23 + i123 / 2,
    TE_T1_T2_T3 = i123
  )
  expect_equal(setNames(estimands$estimand, estimands$inquiry), expected)
})

test_that("two_arm_attrition's effect on reporting is the share moved across the threshold", {
  skip_if_not_installed("DeclareDesign")

  d <- make_design("two_arm_attrition", a_R = 0.2, b_R = 0.7)
  draw <- draw_one(d)
  dat <- draw$data
  estimands <- draw$estimands
  # R = 1 when a_R + b_R * Z > u_R, so treatment moves exactly the units with a_R <= u_R < a_R + b_R
  expect_equal(
    estimands$estimand[estimands$inquiry == "ATE on R"],
    mean(dat$u_R >= 0.2 & dat$u_R < 0.9)
  )
  expect_equal(
    estimands$estimand[estimands$inquiry == "ATE on Y"],
    mean(dat$u_Y >= 0 & dat$u_Y < 1)
  )
})

test_that("randomized_response's estimand is the prevalence its truthful answers reveal", {
  skip_if_not_installed("DeclareDesign")

  d <- make_design("randomized_response")
  draw <- draw_one(d)
  dat <- draw$data
  expect_true(all(dat$Y_Z_Yes == 1))
  expect_true(all(dat$withholder <= dat$sensitive_trait))
  expect_equal(draw$estimands$estimand, mean(dat$Y_Z_Truth))
})

test_that("mediation_analysis's natural outcomes are its potential outcomes at the untreated or treated mediator", {
  skip_if_not_installed("DeclareDesign")

  b <- 0.4; c <- 0.3; d_eff <- 0.5
  d <- make_design(design = "mediation_analysis", b = b, c = c, d = d_eff)
  draw <- draw_one(d)
  dat <- draw$data
  expect_equal(dat$Y_nat0_Z_1, ifelse(dat$M_Z_0 == 1, dat$Y_M_1_Z_1, dat$Y_M_0_Z_1))
  expect_equal(dat$Y_nat1_Z_0, ifelse(dat$M_Z_1 == 1, dat$Y_M_1_Z_0, dat$Y_M_0_Z_0))
  estimands <- draw$estimands
  expected <- c(
    FirstStage = mean(dat$e1 > -1 & dat$e1 <= 0),
    # The Indirect inquiries are the effect of M on Y holding Z fixed, as the Stage 2 estimator targets
    Indirect_0 = b,
    Indirect_1 = b + c,
    Controlled_Direct_0 = d_eff,
    Controlled_Direct_1 = d_eff + c,
    Natural_Direct_0 = d_eff + c * mean(dat$M_Z_0),
    Natural_Direct_1 = d_eff + c * mean(dat$M_Z_1)
  )
  expect_equal(setNames(estimands$estimand, estimands$inquiry), expected)
})
