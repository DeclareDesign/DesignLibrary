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

test_that("install_library_dependencies dry run plans without installing", {
  res <- install_library_dependencies(
    include_shiny = FALSE,
    include_suggests = FALSE,
    verbose = FALSE,
    dry_run = TRUE
  )
  expect_length(res$installed, 0L)
  expect_length(res$failed, 0L)
  expect_true(all(c("DeclareDesign", "yaml") %in% res$already_ok))
  expect_false(any(c("DeclareDesign", "yaml") %in% res$to_install))
  missing <- setdiff(DesignLibrary:::design_declared_packages(), res$already_ok)
  expect_setequal(intersect(res$to_install, DesignLibrary:::design_declared_packages()), missing)
})
