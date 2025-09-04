context("Redesign")

test_that("redesign check", {

  design_1 <- two_arm_designer(assignment_prob  = 0)
  design_2 <- redesign(design_1, assignment_prob  = 1)

  expect_true( all((design_1 |> draw_data())$Z == 1- (design_2 |> draw_data())$Z))
})