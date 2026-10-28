test_that("the app opens on a default design and the banner picker switches it", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  app <- shiny::shinyAppDir(shiny_app_dir())

  shiny::testServer(app, {
    expect_equal(selected_id(), "two_arm_simple")
    expect_false(user_picked())
    header <- output$selected_header$html
    expect_match(header, "by default")
    expect_match(header, "pick_back_library")

    session$setInputs(pick_back_library_diag = "multiarm_trial")
    expect_equal(selected_id(), "multiarm_trial")
    expect_true(user_picked())
    expect_no_match(output$selected_header$html, "by default")
  })
})
