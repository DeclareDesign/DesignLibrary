test_that("copy_library_shiny writes a standalone app folder", {
  skip_if_not(requireNamespace("withr", quietly = TRUE))
  withr::with_tempdir({
    dest <- copy_library_shiny("shiny-out")
    expect_true(file.exists(file.path(dest, "app.R")))
    expect_true(file.exists(file.path(dest, "deploy-options.R")))
    expect_true(file.exists(file.path(dest, "local.R.example")))
    # local.R is preserved across recopies
    writeLines("options(designlibrary.keep = TRUE)", file.path(dest, "local.R"))
    copy_library_shiny(dest, overwrite = TRUE)
    expect_true(file.exists(file.path(dest, "local.R")))
    expect_match(paste(readLines(file.path(dest, "local.R")), collapse = "\n"), "keep = TRUE")
  })
})

test_that("install_library_dependencies reports already-present packages", {
  skip_if_not_installed("yaml")
  res <- install_library_dependencies(
    include_shiny = FALSE,
    include_suggests = FALSE,
    verbose = FALSE
  )
  expect_true(is.list(res))
  expect_true("yaml" %in% res$already_ok || "yaml" %in% res$installed)
  # Optional design extras (cjoint, MatchIt, …) may fail on CRAN/offline hosts
  if (length(res$failed)) {
    message("install_library_dependencies failed (ok to skip): ", paste(res$failed, collapse = ", "))
  }
  expect_true("yaml" %in% res$already_ok || "yaml" %in% res$installed)
})
