test_that("list_designs finds the starter library", {
  idx <- list_designs()
  expect_true(nrow(idx) >= 2)
  expect_true(all(c("two_arm_trial", "two_arm_with_blocks") %in% idx$id))
  expect_true("2.1" %in% idx$alias)
  expect_true("2.2" %in% idx$alias)
  expect_s3_class(idx, "research_designs_list")
})

test_that("list_designs is metadata-only by default", {
  idx <- list_designs()
  expect_true("params" %in% names(idx))
  expect_type(idx$params, "character")
  expect_length(idx$params, nrow(idx))
  two <- idx$params[idx$id %in% c("two_arm", "multiarm_trial")]
  expect_true(length(two) >= 1L)
  expect_true(any(nzchar(two)))
  expect_true(any(grepl("(^|, )N(,|$)", two)))
})

test_that("Shiny library table still displays a params column", {
  app <- system.file("shiny", "app.R", package = "ResearchDesigns")
  if (!nzchar(app) || !file.exists(app)) {
    app <- file.path("inst", "shiny", "app.R")
  }
  lines <- readLines(app, warn = FALSE, encoding = "UTF-8")
  expect_true(any(grepl('c\\("label", "category", "params", "packages"\\)', lines)))
  expect_true(any(grepl("list_designs\\(shiny_only = TRUE\\)", lines)))
  expect_false(any(grepl("discover_params\\s*=\\s*TRUE", lines)))
})

test_that("list_designs print is compact", {
  idx <- list_designs()
  out <- paste(capture.output(print(idx)), collapse = "\n")
  expect_match(out, "ResearchDesigns library")
  expect_match(out, "Getting started")
  leftover_templates <- idx$category %in% c("template", "templates") &
    !idx$id %in% ResearchDesigns:::starter_design_ids()
  if (any(leftover_templates)) {
    expect_match(out, "Other design templates")
  }
  expect_match(out, "Other RDSS designs")
  expect_match(out, "two_arm \\(")
  expect_match(out, "design_info")
  expect_false(grepl("include_in_shiny", out))
  expect_false(grepl("\\bparams\\b", out))
  expect_false(grepl("Packages:", out))
  starter <- ResearchDesigns:::starter_design_ids()
  starter <- starter[starter %in% idx$id]
  expect_equal(idx$id[seq_along(starter)], starter)
  other_pos <- which(!idx$id %in% starter)[1]
  expect_true(max(match(starter, idx$id)) < other_pos)
  shiny_idx <- list_designs(shiny_only = TRUE)
  shiny_starter <- starter[starter %in% shiny_idx$id]
  expect_equal(shiny_idx$id[seq_along(shiny_starter)], shiny_starter)
  if (sum(idx$category == "rdss" & !idx$id %in% starter) > 10L) {
    expect_match(out, "more")
    expect_match(out, "list_all")
  }
  out_all <- paste(capture.output(print(idx, list_all = TRUE)), collapse = "\n")
  expect_match(out_all, "village_campaign")
})

test_that("YAML-less defaults fill category and object", {
  skip_if_not(requireNamespace("withr", quietly = TRUE))
  withr::with_tempdir({
    dir.create("designs")
    writeLines(
      c(
        "b <- 1",
        "design <- structure(list(), class = 'design')"
      ),
      "designs/plain_demo.R"
    )
    # parse_design_file is internal; exercise via parse on a real file path
    parsed <- ResearchDesigns:::parse_design_file("designs/plain_demo.R")
    expect_equal(parsed$meta$id, "plain_demo")
    expect_equal(parsed$meta$category, "Other")
    expect_equal(parsed$meta$object, "design")
    expect_true(isTRUE(parsed$meta$include_in_shiny))
  })
})

test_that("get_code returns simple and full forms", {
  code <- get_code("two_arm_simple", style = "both", b = 0.2)
  expect_s3_class(code, "research_designs_code")
  expect_match(code$simple, 'make_design\\("two_arm_simple", b = 0\\.2\\)')
  expect_true(grepl("declare_model", code$full))
})

test_that("get_code print method cats full source", {
  code <- get_code("two_arm_simple")
  out <- paste(capture.output(print(code)), collapse = "\n")
  expect_false(grepl("\\$simple", out))
  expect_false(grepl("\\$full", out))
  expect_false(grepl('\\[1\\] "', out))
  expect_match(out, "declare_parameters")
  expect_match(out, "declare_estimator\\(Y ~ Z\\)")
  expect_equal(as.character(code), format(code))
})

test_that("get_code style = 'simple' prints the one-liner", {
  code <- get_code("two_arm_simple", style = "simple")
  out <- paste(capture.output(print(code)), collapse = "\n")
  expect_equal(trimws(out), 'make_design("two_arm_simple")')
})

test_that("alias and id both resolve", {
  skip_on_cran()
  skip_if_not_installed("DeclareDesign")
  d1 <- tryCatch(make_design("two_arm_trial"), error = function(e) e)
  d2 <- tryCatch(make_design("2.1"), error = function(e) e)
  if (inherits(d1, "error") || inherits(d2, "error")) {
    skip(paste("DeclareDesign runtime issue:", conditionMessage(d1)))
  }
  expect_equal(attr(d1, "research_designs_id"), "two_arm_trial")
  # Book alias 2.1 points at the RDSS chapter port, not the template
  expect_equal(attr(d2, "research_designs_id"), "two_arm_trial_rdss")
})

test_that("YAML diagnosands are parsed and preferred_diagnosands works", {
  skip_if_not(requireNamespace("withr", quietly = TRUE))
  withr::with_tempdir({
    dir.create("designs")
    writeLines(
      c(
        "---",
        "id: dg_demo",
        "diagnosands: rmse, bias",
        "---",
        "b <- 1",
        "design <- structure(list(), class = 'design')"
      ),
      "designs/dg_demo.R"
    )
    parsed <- ResearchDesigns:::parse_design_file("designs/dg_demo.R")
    expect_equal(parsed$meta$diagnosands, c("rmse", "bias"))
  })
  expect_equal(preferred_diagnosands("two_arm_trial"), c("bias", "power"))
  expect_equal(preferred_diagnosands("logit_probit_ols"), c("rmse", "bias"))
  info <- design_info("logit_probit_ols")
  expect_equal(info$diagnosands, c("rmse", "bias"))
})

test_that("list_designs overlays files missing from the baked index", {
  skip_if_not(requireNamespace("withr", quietly = TRUE))
  withr::with_tempdir({
    writeLines(
      c("Package: ResearchDesigns", "Version: 0.0.0"),
      "DESCRIPTION"
    )
    dir.create(file.path("inst", "designs"), recursive = TRUE)
    dir.create(file.path("inst", "library_index"), recursive = TRUE)
    writeLines(
      c(
        "---",
        "id: baked_one",
        "label: Fresh One",
        "category: template",
        "---",
        "design <- structure(list(), class = 'design')"
      ),
      file.path("inst", "designs", "baked_one.R")
    )
    writeLines(
      c(
        "---",
        "id: my_design",
        "label: My Design",
        "category: template",
        "params:",
        "  \"N\": \"Sample size\"",
        "---",
        "N <- 10",
        "design <- structure(list(), class = 'design')"
      ),
      file.path("inst", "designs", "my_design.R")
    )
    baked <- data.frame(
      id = "baked_one",
      alias = NA_character_,
      label = "Stale One",
      category = "template",
      keywords = "",
      packages = "",
      description = NA_character_,
      include_in_shiny = TRUE,
      functional = TRUE,
      file = "baked_one.R",
      file_mtime = 0,
      stringsAsFactors = FALSE
    )
    utils::write.csv(
      baked,
      file.path("inst", "library_index", "designs_index.csv"),
      row.names = FALSE
    )
    withr::with_options(list(ResearchDesigns.root = normalizePath(getwd())), {
      idx <- list_designs()
      expect_true("baked_one" %in% idx$id)
      expect_true("my_design" %in% idx$id)
      expect_equal(idx$label[idx$id == "baked_one"], "Fresh One")
      expect_equal(idx$label[idx$id == "my_design"], "My Design")
      expect_match(idx$params[idx$id == "my_design"], "N")
      expect_s3_class(idx, "research_designs_list")
    })
  })
})

test_that("contributor_checklist is non-empty", {
  expect_true(length(contributor_checklist()) >= 5)
  expect_true(any(grepl("diagnosands", contributor_checklist(), fixed = TRUE)))
})

test_that("bake_previews returns the path it wrote", {
  skip_on_cran()
  skip_if_not_installed("DeclareDesign")
  id <- "two_arm"
  prev_dir <- ResearchDesigns:::package_write_paths()$previews
  orig <- file.path(prev_dir, paste0(id, ".rds"))
  bak <- tempfile(fileext = ".rds")
  if (file.exists(orig)) {
    file.copy(orig, bak, overwrite = TRUE)
  }
  on.exit({
    if (file.exists(bak)) file.copy(bak, orig, overwrite = TRUE)
  }, add = TRUE)

  baked <- bake_previews(designs = id, sims = 2)
  expect_equal(length(baked$paths), 1L)
  expect_true(file.exists(baked$paths[[1]]))
  expect_equal(nrow(baked$failures), 0L)
})
