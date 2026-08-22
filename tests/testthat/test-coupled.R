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
    parsed <- ResearchDesigns:::parse_design_file("designs/coupled_demo.R")
    expect_equal(
      parsed$meta$coupled$m_arms,
      c("outcome_means", "outcome_sds", "conditions")
    )
  })
})

test_that("coupled_help_text matches the Shiny / message note", {
  note <- "Changing m_arms requires matching-length outcome_means, outcome_sds, and conditions."
  expect_equal(
    ResearchDesigns:::format_coupled_note(
      "m_arms",
      c("outcome_means", "outcome_sds", "conditions")
    ),
    note
  )
  expect_equal(ResearchDesigns:::coupled_help_text("multiarm_trial"), note)
  expect_equal(length(ResearchDesigns:::coupled_help_text("two_arm_trial")), 0L)

  args <- data.frame(name = "N", kind = "scalar", stringsAsFactors = FALSE)
  html <- ResearchDesigns:::redesign_kind_help("multiarm_trial", args = args)
  expect_match(html, "Note: Changing m_arms requires matching-length")
  html_plain <- ResearchDesigns:::redesign_kind_help("two_arm", args = args)
  expect_false(grepl("Note:", html_plain, fixed = TRUE))
})

test_that("make_design sources at defaults then redesigns", {
  skip_if_not_installed("DeclareDesign")
  skip_on_cran()

  expect_false("overlay" %in% names(formals(ResearchDesigns:::eval_design)))
  ns <- asNamespace("ResearchDesigns")
  expect_false("split_make_design_dots" %in% ls(ns, all.names = TRUE))
  expect_false("is_assign_to" %in% ls(ns, all.names = TRUE))

  expect_message(
    d <- make_design("multiarm_trial", m_arms = 4),
    "Changing m_arms requires matching-length"
  )
  expect_s3_class(d, "design")
  parsed <- ResearchDesigns:::resolve_design("multiarm_trial")
  params <- ResearchDesigns:::discover_design_params(d, code = parsed$code)
  m <- params$value[[match("m_arms", params$name)]]
  om <- params$value[[match("outcome_means", params$name)]]
  expect_equal(as.integer(m[[1]]), 4L)
  expect_equal(length(om), 3L)
})

test_that("design_info print shows coupled notes", {
  skip_if_not_installed("DeclareDesign")
  skip_on_cran()

  out <- paste(capture.output(print(design_info("multiarm_trial"))), collapse = "\n")
  expect_match(out, "Coupled parameters")
  expect_match(
    out,
    "Changing m_arms requires matching-length outcome_means, outcome_sds, and conditions"
  )
  out_plain <- paste(capture.output(print(design_info("two_arm_trial"))), collapse = "\n")
  expect_false(grepl("Coupled parameters", out_plain))
})
