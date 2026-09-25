# Each designer is tested in its own file, which must run the shared contract.
# A designer added without one fails here rather than going untested.

test_that("every exported designer has a test file that runs the contract", {
  exports <- getNamespaceExports("DesignLibrary")
  designers <- sort(exports[grepl("_designer$", exports) & !grepl("^simple_", exports)])
  expect_length(designers, 15)
  for (designer in designers) {
    path <- test_path(paste0("test-", designer, ".R"))
    expect_true(file.exists(path), label = path)
    if (file.exists(path)) {
      expect_match(paste(readLines(path), collapse = "\n"),
                   paste0('test_designer_contract("', designer, '"'), fixed = TRUE,
                   label = path)
    }
  }
})
