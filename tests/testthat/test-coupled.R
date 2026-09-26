test_that("YAML coupled: is stored on meta", {
  skip_if_not(requireNamespace("withr", quietly = TRUE))
  withr::with_tempdir({
    dir.create("designs")
    writeLines(
      c(
        "---",
        "id: coupled_demo",
        "coupled:",
        "  m_arms: [outcome_means, outcome_sds, conditions]",
        "---",
        "m_arms <- 3",
        "design <- structure(list(), class = 'design')"
      ),
      "designs/coupled_demo.R"
    )
    parsed <- DesignLibrary:::parse_design_file("designs/coupled_demo.R")
    expect_equal(
      parsed$meta$coupled$m_arms,
      c("outcome_means", "outcome_sds", "conditions")
    )
  })
})

test_that("coupled_help_text matches the Shiny / message note", {
  note <- "Changing m_arms requires matching-length outcome_means, outcome_sds, and conditions."
  expect_equal(
    DesignLibrary:::format_coupled_note(
      "m_arms",
      c("outcome_means", "outcome_sds", "conditions")
    ),
    note
  )
  expect_equal(
    DesignLibrary:::coupled_help_text("cluster_random_sampling"),
    "Changing ICC requires matching-length locality_shock and individual_shock."
  )
  expect_equal(length(DesignLibrary:::coupled_help_text("two_arm_simple")), 0L)
  expect_equal(length(DesignLibrary:::coupled_help_text("multiarm_trial")), 0L)

  args <- data.frame(name = "N", kind = "scalar", stringsAsFactors = FALSE)
  html <- DesignLibrary:::redesign_kind_help("cluster_random_sampling", args = args)
  expect_match(html, "Note: Changing ICC requires matching-length")
  html_plain <- DesignLibrary:::redesign_kind_help("two_arm_flexible", args = args)
  expect_false(grepl("Note:", html_plain, fixed = TRUE))
})

test_that("make_design sources at defaults then redesigns", {
  skip_if_not_installed("DeclareDesign")
  skip_on_cran()

  expect_false("overlay" %in% names(formals(DesignLibrary:::eval_design)))
  ns <- asNamespace("DesignLibrary")
  expect_false("split_make_design_dots" %in% ls(ns, all.names = TRUE))
  expect_false("is_assign_to" %in% ls(ns, all.names = TRUE))

  expect_no_message(d <- make_design("multiarm_trial", m_arms = 4))
  expect_s3_class(d, "design")
  parsed <- DesignLibrary:::resolve_design("multiarm_trial")
  params <- DesignLibrary:::discover_design_params(d, code = parsed$code)
  m <- params$value[[match("m_arms", params$name)]]
  expect_equal(as.integer(m[[1]]), 4L)
})

test_that("multiarm_trial extends its default vectors to m_arms", {
  skip_if_not_installed("DeclareDesign")
  skip_on_cran()

  d <- make_design("multiarm_trial", m_arms = 5)
  expect_equal(sort(unique(as.integer(DeclareDesign::draw_data(d)$Z))), 1:5)
  est <- DeclareDesign::draw_estimates(d)
  expect_equal(est$inquiry, paste0("ate_Y_", 2:5, "_1"))
  expect_equal(est$term, paste0("factor(Z)", 2:5))

  d4 <- make_design("multiarm_trial", m_arms = 4, outcome_means = c(0, 0.5, 1, 2))
  expect_equal(DeclareDesign::draw_estimands(d4)$estimand, c(0.5, 1, 2))

  bad <- make_design("multiarm_trial", m_arms = 4, outcome_means = c(0, 1, 2))
  expect_error(DeclareDesign::draw_data(bad), "outcome_means has length 3 but m_arms is 4")
})

test_that("design_info print shows coupled notes", {
  skip_if_not_installed("DeclareDesign")
  skip_on_cran()

  out <- paste(capture.output(print(design_info("cluster_random_sampling"))), collapse = "\n")
  expect_match(out, "Coupled parameters")
  expect_match(
    out,
    "Changing ICC requires matching-length locality_shock and individual_shock"
  )
  out_plain <- paste(capture.output(print(design_info("two_arm_simple"))), collapse = "\n")
  expect_false(grepl("Coupled parameters", out_plain))
})
